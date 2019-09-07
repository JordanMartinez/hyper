module Hyper.Node.Server
       ( HttpRequest
       , NodeResponse
       , Hyper
       , writeString
       , write
       , module Hyper.Node.Server.Options
       , runServer
       -- , runServer'
       ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, runReaderT)
import Data.Either (Either(..), either)
import Data.HTTP.Method as Method
import Data.Int as Int
import Data.Indexed (Indexed(..))
import Data.Lazy (defer)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_, makeAff, nonCanceler, runAff_)
import Effect.Aff.AVar (empty, new, put, take)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (catchException, Error)
import Foreign.Object as Object
import Hyper.Conn (BodyOpen, BodyRead, BodyUnread, Conn, HeadersOpen, ResponseEnded, StatusLineOpen, kind RequestState, kind ResponseState)
import Hyper.Header (Header)
import Hyper.Node.Server.Options (Hostname(..), Options, Port(..), defaultOptions, defaultOptionsWithLogging) as Hyper.Node.Server.Options
import Hyper.Node.Server.Options (Options)
import Hyper.Request (class ReadableBody, class Request, class StreamableBody, RequestData, parseUrl)
import Hyper.Response (class Response, class ResponseWritable)
import Hyper.Status (Status(..))
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.HTTP as HTTP
import Node.Stream (Stream, Writable)
import Node.Stream as Stream

-- Note to self: this is the real 'runServer'. The other one is
-- a temporary reduction until I specify monad transformers on top of
-- the indexed monad
-- runServer
--   :: forall m (endingReqState :: RequestState)
--    . IxMonad m
--   => Options
--   -> m (Conn BodyUnread StatusLineOpen) (Conn endingReqState ResponseEnded) Unit
--   -> Effect Unit
-- runServer = runServer' identity

runServer
  :: forall (endingReqState :: RequestState)
   . Options
  -> Hyper (ReaderT HttpConn Aff) (Conn BodyUnread StatusLineOpen) (Conn endingReqState ResponseEnded) Unit
  -> Effect Unit
runServer options middleware = do
  server <- HTTP.createServer onRequest
  let listenOptions = { port: unwrap options.port
                      , hostname: unwrap options.hostname
                      , backlog: Nothing
                      }
  HTTP.listen server listenOptions (options.onListening options.hostname options.port)
  where
    onRequest :: HTTP.Request -> HTTP.Response -> Effect Unit
    onRequest request response =
      runAff_ callback (runReaderT (runHyper middleware) connection)
      where
        connection = { request: mkHttpRequest request, response }

        callback :: Either Error _ -> Effect Unit
        callback = case _ of
          Left err -> options.onRequestError err
          Right _ -> pure unit

data HttpRequest = HttpRequest HTTP.Request RequestData

type HttpConn = { request :: HttpRequest, response :: HTTP.Response }

mkHttpRequest :: HTTP.Request -> HttpRequest
mkHttpRequest request =
  HttpRequest request requestData
  where
    headers = HTTP.requestHeaders request
    requestData =
      { url: HTTP.requestURL request
      , parsedUrl: defer \_ -> parseUrl (HTTP.requestURL request)
      , headers: headers
      , method: Method.fromString (HTTP.requestMethod request)
      , contentLength: Object.lookup "content-length" headers
                      >>= Int.fromString
      }

-- Helper function that reads a Stream into a Buffer, and throws error
-- in `Aff` when failed.
readBodyAsBuffer
  :: HttpRequest
  -> Aff Buffer
readBodyAsBuffer (HttpRequest request _) = do
  let stream = HTTP.requestAsStream request
  bodyResult <- empty
  chunks <- new []
  fillResult <- liftEffect $
    catchException (pure <<< Left) (Right <$> fillBody stream chunks bodyResult)
  -- Await the body, or an error.
  body <- take bodyResult
  -- Return the body, if neither `fillResult` nor `body` is a `Left`.
  either throwError pure (fillResult *> body)
  where
    fillBody stream chunks bodyResult = do
      -- Append all chunks to the body buffer.
      Stream.onData stream \chunk ->
        let modification = do
              v <- take chunks
              put (v <> [chunk]) chunks
        in void (launchAff modification)
      -- Complete with `Left` on error.
      Stream.onError stream $
        launchAff_ <<< flip put bodyResult <<< Left
      -- Complete with `Right` on successful "end" event.
      Stream.onEnd stream $ void $ launchAff $
        take chunks
        >>= concat'
        >>= (pure <<< Right)
        >>= flip put bodyResult
    concat' = liftEffect <<< Buffer.concat

instance requestHyper :: (MonadAsk HttpConn m, MonadAff m) => Request (Hyper m) where
  getRequestData
    :: forall (reqState :: RequestState) (resState :: ResponseState)
     . Hyper m (Conn reqState resState) (Conn reqState resState) RequestData
  getRequestData = Hyper $ Indexed do
    { request: HttpRequest _ d } <- ask
    pure d

  ignoreBody
    :: forall (resState :: ResponseState)
     . Hyper m (Conn BodyUnread resState) (Conn BodyRead resState) Unit
  ignoreBody = Hyper $ Indexed (pure unit)

instance readableBodyHyperBuffer :: (MonadAsk HttpConn m, MonadAff m) => ReadableBody (Hyper m) Buffer where
  readBody
    :: forall (resState :: ResponseState)
     . Hyper m (Conn BodyUnread resState) (Conn BodyRead resState) Buffer
  readBody = Hyper $ Indexed do
    { request } <- ask
    liftAff $ readBodyAsBuffer request
else
instance readableBodyHyperString :: (MonadAsk HttpConn m, MonadAff m) => ReadableBody (Hyper m) String where
  readBody
    :: forall (resState :: ResponseState)
     . Hyper m (Conn BodyUnread resState) (Conn BodyRead resState) String
  readBody = Hyper $ Indexed do
    { request } <- ask
    buf <- liftAff $ readBodyAsBuffer request
    liftEffect $ Buffer.toString UTF8 buf

instance streamableBodyHyper :: (MonadAsk HttpConn m, MonadAff m) => StreamableBody (Hyper m) m (Stream (read :: Stream.Read)) where
  streamBody
    :: forall (resState :: ResponseState)
     . (Stream (read :: Stream.Read) -> m Unit)
     -> Hyper m (Conn BodyUnread resState) (Conn BodyRead resState) Unit
  streamBody useStream = Hyper $ Indexed do
    { request: HttpRequest request _ } <- ask
    useStream (HTTP.requestAsStream request)

-- newtype WriterResponse rw r (resState :: ResponseState) =
--   WriterResponse { writer :: rw | r }
--
-- getWriter :: forall req c m rw r reqState (resState :: ResponseState).
--             Monad m =>
--             NoTransition m req reqState (WriterResponse rw r) resState c rw
-- getWriter = getConn <#> \{ response: WriterResponse rec } -> rec.writer

-- A limited version of Writable () e, with which you can only write, not end,
-- the Stream.
newtype NodeResponse m
  = NodeResponse (Writable () -> m Unit)

writeString :: forall m. MonadAff m => Encoding -> String -> NodeResponse m
writeString enc str = NodeResponse $ \w ->
  liftAff (makeAff (\k -> Stream.writeString w enc str (k (pure unit))
                          *> pure nonCanceler))

write :: forall m. MonadAff m => Buffer -> NodeResponse m
write buffer = NodeResponse $ \w ->
  liftAff (makeAff (\k -> Stream.write w buffer (k (pure unit))
                          *> pure nonCanceler))

instance responseHyper :: (MonadAsk HttpConn m, MonadAff m) => Response (Hyper m) (NodeResponse m) where
  writeStatus
    :: forall (reqState :: RequestState)
     . Status
    -> Hyper m (Conn reqState StatusLineOpen) (Conn reqState HeadersOpen) Unit
  writeStatus (Status { code, reasonPhrase }) = Hyper $ Indexed do
    { response } <- ask
    liftEffect do
      HTTP.setStatusCode response code
      HTTP.setStatusMessage response reasonPhrase

  writeHeader
    :: forall (reqState :: RequestState)
     . Header
    -> Hyper m (Conn reqState HeadersOpen) (Conn reqState HeadersOpen) Unit
  writeHeader (Tuple name value) = Hyper $ Indexed do
    { response } <- ask
    liftEffect $ HTTP.setHeader response name value

  closeHeaders
    :: forall (reqState :: RequestState)
     . Hyper m (Conn reqState HeadersOpen) (Conn reqState BodyOpen) Unit
  closeHeaders = Hyper $ Indexed (pure unit)

  send
    :: forall (reqState :: RequestState)
     . NodeResponse m
    -> Hyper m (Conn reqState BodyOpen) (Conn reqState BodyOpen) Unit
  send (NodeResponse f) = Hyper $ Indexed do
    { response } <- ask
    f (HTTP.responseAsStream response)

  end
    :: forall (reqState :: RequestState)
     . Hyper m (Conn reqState BodyOpen) (Conn reqState ResponseEnded) Unit
  end = Hyper $ Indexed do
    { response } <- ask
    liftEffect (Stream.end (HTTP.responseAsStream response) (pure unit))

instance responseWritableStringToNodeResponse :: MonadAff m => ResponseWritable (Hyper m) String (NodeResponse m) where
  toResponse = Hyper <<< Indexed <<< pure <<< writeString UTF8

instance responseWritableStringEncodingToNodeResponse :: MonadAff m => ResponseWritable (Hyper m) (Tuple Encoding String) (NodeResponse m) where
  toResponse (Tuple encoding body) = Hyper (Indexed (pure (writeString encoding body)))

instance responseWritableBufferToNodeResponse :: MonadAff m => ResponseWritable (Hyper m) Buffer (NodeResponse m) where
  toResponse = Hyper <<< Indexed <<< pure <<< write

newtype Hyper m x y a = Hyper (Indexed m x y a)

runHyper :: forall m x y a. Monad m => Hyper m x y a -> m a
runHyper (Hyper (Indexed ma)) = ma

derive newtype instance ixFunctorHyper ∷ Functor m ⇒ IxFunctor (Hyper m)
derive newtype instance ixApplyHyper ∷ Apply m ⇒ IxApply (Hyper m)
derive newtype instance ixApplicativeHyper ∷ Applicative m ⇒ IxApplicative (Hyper m)
derive newtype instance ixBindHyper ∷ Bind m ⇒ IxBind (Hyper m)
derive newtype instance ixMonadHyper ∷ Monad m ⇒ IxMonad (Hyper m)
