module Hyper.Response where

import Prelude

import Control.Monad.Indexed (class IxMonad)
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed.Reader.Trans (IxReaderT)
import Control.Monad.Indexed.State.Trans (IxStateT)
import Control.Monad.Indexed.Trans.Class (ilift)
import Control.Monad.Indexed.Writer.Trans (IxWriterT)
import Data.Foldable (class Foldable, traverse_)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
import Hyper.Conn (type (&), BodyOpen, HeadersOpen, ResponseEnded, StatusLineOpen, kind ResponseState)
import Hyper.Header (Header)
import Hyper.Status (Status, statusFound)

-- | The operations that a response writer, provided by the server backend,
-- | must support.
class Response m body | m -> body where
  writeStatus
    :: forall reqState
     . Status
    -> m (reqState & StatusLineOpen) (reqState & HeadersOpen) Unit
  writeHeader
    :: forall reqState
     . Header
    -> m (reqState & HeadersOpen) (reqState & HeadersOpen) Unit
  closeHeaders
    :: forall reqState
     . m (reqState & HeadersOpen) (reqState & BodyOpen) Unit
  send
    :: forall reqState
     . body
    -> m (reqState & BodyOpen) (reqState & BodyOpen) Unit
  end
    :: forall reqState
     . m (reqState & BodyOpen) (reqState & ResponseEnded) Unit

instance responseIxMonadReaderT :: (IxMonad m, Response m body) => Response (IxReaderT r m) body where
    writeStatus = ilift <<< writeStatus
    writeHeader = ilift <<< writeHeader
    closeHeaders = ilift closeHeaders
    send = ilift <<< send
    end = ilift end

instance responseIxMonadStateT :: (IxMonad m, Response m body) => Response (IxStateT r m) body where
    writeStatus = ilift <<< writeStatus
    writeHeader = ilift <<< writeHeader
    closeHeaders = ilift closeHeaders
    send = ilift <<< send
    end = ilift end

instance responseIxMonadWriterT :: (Monoid w, IxMonad m, Response m body) => Response (IxWriterT w m) body where
    writeStatus = ilift <<< writeStatus
    writeHeader = ilift <<< writeHeader
    closeHeaders = ilift closeHeaders
    send = ilift <<< send
    end = ilift end


headers
  :: forall f m reqState body
  .  Foldable f
  => IxMonad m
  => Applicative (m (reqState & HeadersOpen) (reqState & HeadersOpen))
  => Response m body
  => f Header
  -> m (reqState & HeadersOpen) (reqState & BodyOpen) Unit
headers hs = Ix.do
  traverse_ writeHeader hs
  closeHeaders

contentType
  :: forall m reqState body
  .  IxMonad m
  => Response m body
  => MediaType
  -> m (reqState & HeadersOpen) (reqState & HeadersOpen) Unit
contentType mediaType =
  writeHeader (Tuple "Content-Type" (unwrap mediaType))

redirect
  :: forall m reqState body
  .  IxMonad m
  => Response m body
  => String
  -> m (reqState & StatusLineOpen) (reqState & HeadersOpen) Unit
redirect uri = Ix.do
  writeStatus statusFound
  writeHeader (Tuple "Location" uri)

class ResponseWritable m responseData body | m responseData -> body where
  toResponse :: forall i. responseData -> m i i body

instance responseWritableIxMonadReaderT :: (IxMonad m, ResponseWritable m r body) => ResponseWritable (IxReaderT rr m) r body where
  toResponse = ilift <<< toResponse

instance responseWritableIxMonadStateT :: (IxMonad m, ResponseWritable m r body) => ResponseWritable (IxStateT s m) r body where
  toResponse = ilift <<< toResponse

instance responseWritableIxMonadWriterT :: (Monoid w, IxMonad m, ResponseWritable m r body) => ResponseWritable (IxWriterT w m) r body where
  toResponse = ilift <<< toResponse

respond
  :: forall m r body reqState
  .  IxMonad m
  => ResponseWritable m r body
  => Response m body
  => r
  -> m (reqState & BodyOpen) (reqState & ResponseEnded) Unit
respond r = Ix.do
  resp <- toResponse r
  send resp
  end
