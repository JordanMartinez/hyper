module Hyper.Node.Server
       ( HttpRequest
       , HttpConn
       -- don't export constructor so users can't end stream prematurely
       , NodeResponse

       -- don't export constructor so users can't change Conn status
       , Hyper
       , writeString
       , write
       , createServer
       , createServerSecure
       ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.Either (Either(..), either)
import Data.HTTP.Method as Method
import Data.Indexed (Indexed(..))
import Data.Options (Options)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff, launchAff_, makeAff, nonCanceler, runAff_)
import Effect.Aff.AVar (empty, new, put, take)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Indexed.Class (class IxMonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (catchException)
import Effect.Indexed.Class (class IxMonadEffect)
import Hyper.Conn (BodyUnread, type (&), ResponseEnded, StatusLineOpen, kind RequestState)
import Hyper.Request (class ReadableBody, class Request, class StreamableBody, RequestData)
import Hyper.Response (class ResponseWritable, class Response)
import Hyper.Status (Status(..))
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.HTTP (Server)
import Node.HTTP as HTTP
import Node.HTTP.Secure (SSLOptions)
import Node.HTTP.Secure as HTTPS
import Node.Stream (Stream, Writable)
import Node.Stream as Stream

onHttpRequest
  :: forall (endingReqState :: RequestState)
   . (Error -> Effect Unit)
  -> Hyper (BodyUnread & StatusLineOpen) (endingReqState & ResponseEnded) Unit
  -> HTTP.Request
  -> HTTP.Response
  -> Effect Unit
onHttpRequest onRequestError middleware request response =
  runAff_ callback (runHyper middleware conn)
  where
    conn = HttpConn { request: mkHttpRequest request, response: response }

    callback = case _ of
      Left err -> onRequestError err
      Right _ -> pure unit

createServer :: forall (endingReqState :: RequestState)
              . (Error -> Effect Unit)
             -> Hyper (BodyUnread & StatusLineOpen) (endingReqState & ResponseEnded) Unit
             -> Effect Server
createServer onRequestError middleware =
  HTTP.createServer (onHttpRequest onRequestError middleware)

createServerSecure :: forall (endingReqState :: RequestState)
              . Options SSLOptions
             -> (Error -> Effect Unit)
             -> Hyper (BodyUnread & StatusLineOpen) (endingReqState & ResponseEnded) Unit
             -> Effect Server
createServerSecure sslOptions onRequestError middleware =
  HTTPS.createServer sslOptions (onHttpRequest onRequestError middleware)

newtype HttpConn = HttpConn { request :: HttpRequest
                            , response :: HTTP.Response
                            }

data HttpRequest
  = HttpRequest HTTP.Request RequestData

mkHttpRequest :: HTTP.Request -> HttpRequest
mkHttpRequest request =
  HttpRequest request requestData
  where
    requestData =
      { url: HTTP.requestURL request
      , headers: HTTP.requestHeaders request
      , method: Method.fromString (HTTP.requestMethod request)
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

newtype Hyper from to a =
  Hyper (Indexed (ReaderT HttpConn Aff) from to a)

derive newtype instance functorHyper :: Functor (Hyper x x)
derive newtype instance applyHyper :: Apply (Hyper x x)
derive newtype instance applicativeHyper :: Applicative (Hyper x x)
derive newtype instance bindHyper :: Bind (Hyper x x)
derive newtype instance monadHyper :: Monad (Hyper x x)
instance monadEffectHyper :: MonadEffect (Hyper x x) where
  liftEffect = Hyper <<< Indexed <<< liftEffect
instance monadAffHyper :: MonadAff (Hyper x x) where
  liftAff = Hyper <<< Indexed <<< liftAff

derive newtype instance ixFunctorHyper :: IxFunctor Hyper
derive newtype instance ixApplyHyper :: IxApply Hyper
derive newtype instance ixApplicativeHyper :: IxApplicative Hyper
derive newtype instance ixBindHyper :: IxBind Hyper
derive newtype instance ixMonad :: IxMonad Hyper
derive newtype instance ixMonadEffect :: IxMonadEffect Hyper
derive newtype instance ixMonadAff :: IxMonadAff Hyper

runHyper :: forall fromReq fromRes toReq toRes a
          . Hyper (fromReq & fromRes) (toReq & toRes) a
         -> HttpConn -> Aff a
runHyper (Hyper (Indexed readerT)) conn = runReaderT readerT conn

instance requestHyper :: Monad m => Request Hyper where
  getRequestData = Hyper $ Indexed do
    HttpConn { request: HttpRequest _ d } <- ask
    pure d

  ignoreBody = Hyper (Indexed (pure unit))
instance readableBodyHttpRequestString :: (MonadAff m)
                                       => ReadableBody Hyper String where
  readBody = Hyper $ Indexed do
    HttpConn { request } <- ask
    buf <- liftAff (readBodyAsBuffer request)
    liftEffect $ Buffer.toString UTF8 buf
else
instance readableBodyHyperBuffer :: (MonadAff m)
                                       => ReadableBody Hyper Buffer where
  readBody = Hyper $ Indexed do
    HttpConn { request } <- ask
    liftAff (readBodyAsBuffer request)

instance streamableBodyHperAffStream :: MonadAff m
                                           => StreamableBody
                                              Hyper
                                              Aff
                                              (Stream (read :: Stream.Read)) where
  streamBody useStream = Hyper $ Indexed do
    HttpConn { request: HttpRequest request _ } <- ask
    liftAff (useStream (HTTP.requestAsStream request))

instance responseWriterHyperNodeResponse :: Response Hyper (NodeResponse (ReaderT HttpConn Aff)) where
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

instance stringNodeResponse :: MonadAff m => ResponseWritable Hyper String (NodeResponse (ReaderT HttpConn m)) where
  toResponse str = Hyper $ Indexed do
    pure (writeString UTF8 str)

instance stringAndEncodingNodeResponse :: MonadAff m => ResponseWritable Hyper (Tuple Encoding String) (NodeResponse (ReaderT HttpConn m)) where
  toResponse (Tuple encoding body) = Hyper $ Indexed do
    pure (writeString encoding body)

instance bufferNodeResponse :: MonadAff m => ResponseWritable Hyper Buffer (NodeResponse (ReaderT HttpConn m)) where
  toResponse buf = Hyper $ Indexed do
    pure (write buf)
