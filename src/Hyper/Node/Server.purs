module Hyper.Node.Server
       ( HttpRequest
       , HttpConn
       -- don't export constructor so users can't end stream prematurely
       , NodeResponse

       -- don't export constructor so users can't change Conn status
       , Hyper
       , writeString
       , write
       , module Hyper.Node.Server.Options
       , runServer
       , runServer'
       ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Either (Either(..), either)
import Data.HTTP.Method as Method
import Data.Indexed (Indexed(..))
import Data.Int as Int
import Data.Lazy (defer)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_, makeAff, nonCanceler, runAff_)
import Effect.Aff.AVar (empty, new, put, take)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Indexed.Class (class IxMonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Indexed.Class (class IxMonadEffect)
import Effect.Exception (catchException)
import Foreign.Object as Object
import Hyper.Conn (BodyUnread, Conn, ResponseEnded, StatusLineOpen, kind RequestState)
import Hyper.Node.Server.Options (Hostname(..), Options, Port(..), defaultOptions, defaultOptionsWithLogging) as Hyper.Node.Server.Options
import Hyper.Node.Server.Options (Options)
import Hyper.Request (class ReadableBody, class Request, class StreamableBody, RequestData, parseUrl)
import Hyper.Response (class ResponseWritable, class Response)
import Hyper.Status (Status(..))
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.HTTP as HTTP
import Node.Stream (Stream, Writable)
import Node.Stream as Stream

runServer
  :: forall (endingReqState :: RequestState)
   . Options
  -> Hyper Aff (Conn BodyUnread StatusLineOpen) (Conn endingReqState ResponseEnded) Unit
  -> Effect Unit
runServer = runServer' identity

runServer'
  :: forall (endingReqState :: RequestState) m
   . IxMonad m
  => (m (Conn BodyUnread StatusLineOpen) (Conn endingReqState ResponseEnded) ~> Hyper Aff (Conn BodyUnread StatusLineOpen) (Conn endingReqState ResponseEnded))
  -> Options
  -> m (Conn BodyUnread StatusLineOpen) (Conn endingReqState ResponseEnded) Unit
  -> Effect Unit
runServer' runApp options middleware = do
  server <- HTTP.createServer onRequest
  let listenOptions = { port: unwrap options.port
                      , hostname: unwrap options.hostname
                      , backlog: Nothing
                      }
  HTTP.listen server listenOptions (options.onListening options.hostname options.port)
  where
    onRequest :: HTTP.Request -> HTTP.Response -> Effect Unit
    onRequest request response =
      runAff_ callback (runHyper (runApp middleware) conn)
      where
        conn = HttpConn { request: mkHttpRequest request
                        , response: response
                        }

        callback =
          case _ of
            Left err -> options.onRequestError err
            Right _ -> pure unit

newtype HttpConn = HttpConn { request :: HttpRequest
                            , response :: HTTP.Response
                            }

data HttpRequest
  = HttpRequest HTTP.Request RequestData

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

-- newtype WriterResponse rw r (resState :: ResponseState) =
--   WriterResponse { writer :: rw | r }
--
-- getWriter :: forall req c m rw r reqState (resState :: ResponseState).
--             Monad m =>
--             NoTransition m req reqState (WriterResponse rw r) resState c rw
-- getWriter = getConn <#> \{ response: WriterResponse rec } -> rec.writer

newtype Hyper m from to a =
  Hyper (Indexed (ReaderT HttpConn m) from to a)

derive newtype instance functorHyper :: Functor m => Functor (Hyper m x x)
derive newtype instance applyHyper :: Apply m => Apply (Hyper m x x)
derive newtype instance applicativeHyper :: Applicative m => Applicative (Hyper m x x)
derive newtype instance bindHyper :: Bind m => Bind (Hyper m x x)
derive newtype instance monadHyper :: Monad m => Monad (Hyper m x x)

derive newtype instance ixFunctorHyper :: Functor m => IxFunctor (Hyper m)
derive newtype instance ixApplyHyper :: Apply m => IxApply (Hyper m)
derive newtype instance ixApplicativeHyper :: Applicative m => IxApplicative (Hyper m)
derive newtype instance ixBindHyper :: Bind m => IxBind (Hyper m)
derive newtype instance ixMonad :: Monad m => IxMonad (Hyper m)
derive newtype instance ixMonadEffect :: MonadEffect m => IxMonadEffect (Hyper m)
derive newtype instance ixMonadAff :: MonadAff m => IxMonadAff (Hyper m)

runHyper :: forall m fromReq fromRes toReq toRes a
          . Hyper m (Conn fromReq fromRes) (Conn toReq toRes) a
         -> HttpConn -> m a
runHyper (Hyper (Indexed readerT)) conn = runReaderT readerT conn

instance requestHyper :: Monad m => Request (Hyper m) where
  getRequestData = Hyper $ Indexed do
    HttpConn { request: HttpRequest _ d } <- ask
    pure d

  ignoreBody = Hyper (Indexed (pure unit))
instance readableBodyHttpRequestString :: (MonadAff m)
                                       => ReadableBody (Hyper m) String where
  readBody = Hyper $ Indexed do
    HttpConn { request } <- ask
    buf <- liftAff (readBodyAsBuffer request)
    liftEffect $ Buffer.toString UTF8 buf
else
instance readableBodyHyperBuffer :: (MonadAff m)
                                       => ReadableBody (Hyper m) Buffer where
  readBody = Hyper $ Indexed do
    HttpConn { request } <- ask
    liftAff (readBodyAsBuffer request)

instance streamableBodyHperAffStream :: MonadAff m
                                           => StreamableBody
                                              (Hyper m)
                                              Aff
                                              (Stream (read :: Stream.Read)) where
  streamBody useStream = Hyper $ Indexed do
    HttpConn { request: HttpRequest request _ } <- ask
    liftAff (useStream (HTTP.requestAsStream request))

instance responseWriterHyperNodeResponse :: MonadAff m
                                    => Response (Hyper m) (NodeResponse (ReaderT HttpConn m)) where
  writeStatus (Status { code, reasonPhrase }) = Hyper $ Indexed do
    HttpConn { response } <- ask
    liftEffect do
      HTTP.setStatusCode response code
      HTTP.setStatusMessage response reasonPhrase

  writeHeader (Tuple name value) = Hyper $ Indexed do
    HttpConn { response } <- ask
    liftEffect $ HTTP.setHeader response name value

  closeHeaders = Hyper (Indexed (pure unit))

  send (NodeResponse f) = Hyper $ Indexed do
    HttpConn { response } <- ask
    f (HTTP.responseAsStream response)

  end = Hyper $ Indexed do
    HttpConn { response } <- ask
    liftEffect (Stream.end (HTTP.responseAsStream response) (pure unit))

instance stringNodeResponse :: MonadAff m => ResponseWritable (Hyper m) String (NodeResponse (ReaderT HttpConn m)) where
  toResponse str = Hyper $ Indexed do
    pure (writeString UTF8 str)

instance stringAndEncodingNodeResponse :: MonadAff m => ResponseWritable (Hyper m) (Tuple Encoding String) (NodeResponse (ReaderT HttpConn m)) where
  toResponse (Tuple encoding body) = Hyper $ Indexed do
    pure (writeString encoding body)

instance bufferNodeResponse :: MonadAff m => ResponseWritable (Hyper m) Buffer (NodeResponse (ReaderT HttpConn m)) where
  toResponse buf = Hyper $ Indexed do
    pure (write buf)
