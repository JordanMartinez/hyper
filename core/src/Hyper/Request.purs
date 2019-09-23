module Hyper.Request where

import Prelude

import Control.Monad.Indexed (class IxMonad)
import Control.Monad.Indexed.Reader.Trans (IxReaderT)
import Control.Monad.Indexed.State.Trans (IxStateT)
import Control.Monad.Indexed.Trans.Class (ilift)
import Control.Monad.Indexed.Writer.Trans (IxWriterT)
import Data.Either (Either)
import Data.HTTP.Method (CustomMethod, Method)
import Foreign.Object (Object)
import Hyper.Conn (BodyRead, BodyUnread, type (&), kind RequestState, kind ResponseState)

type RequestData =
  { url :: String
  , headers :: Object String
  , method :: Either Method CustomMethod
  }

class Request m where
  getRequestData :: forall reqState resState
                  . m (reqState & resState) (reqState & resState) RequestData

  ignoreBody :: forall resState
              . m (BodyUnread & resState) (BodyRead & resState) Unit

-- | A `ReadableBody` instance reads the complete request body as a
-- | value of type `b`. For streaming the request body, see the
-- | [StreamableBody](#streamablebody) class.
class ReadableBody m body | m -> body where
  readBody :: forall resState
            . m (BodyUnread & resState) (BodyRead & resState) body

-- | A `StreamableBody` instance returns a stream of the request body,
-- | of type `stream`. To read the whole body as a value, without
-- | streaming, see the [ReadableBody](#readablebody) class.
class (Monad innerMonad) <= StreamableBody m innerMonad stream | m -> innerMonad stream where
  streamBody :: forall resState
              . (stream -> innerMonad Unit)
             -> m (BodyUnread & resState) (BodyRead & resState) Unit

instance requestIxMonadStateT :: (IxMonad m, Request m) => Request (IxStateT s m) where
  getRequestData = ilift getRequestData

  ignoreBody = ilift ignoreBody

instance readBodyIxMonadStateT :: (IxMonad m, ReadableBody m b) => ReadableBody (IxStateT s m) b where
  readBody = ilift readBody

instance streamableBodyIxMonadStateT :: (IxMonad m, StreamableBody m inner stream) => StreamableBody (IxStateT s m) inner stream where
  streamBody = ilift <<< streamBody

instance requestIxMonadReaderT :: (IxMonad m, Request m) => Request (IxReaderT r m) where
  getRequestData = ilift getRequestData

  ignoreBody = ilift ignoreBody

instance readBodyIxMonadReaderT :: (IxMonad m, ReadableBody m b) => ReadableBody (IxReaderT r m) b where
  readBody = ilift readBody

instance streamableBodyIxMonadReaderT :: (IxMonad m, StreamableBody m inner stream) => StreamableBody (IxReaderT r m) inner stream where
  streamBody = ilift <<< streamBody

instance requestIxMonadWriterT :: (Monoid w, IxMonad m, Request m) => Request (IxWriterT w m) where
  getRequestData = ilift getRequestData

  ignoreBody = ilift ignoreBody

instance readBodyIxMonadWriterT :: (Monoid w, IxMonad m, ReadableBody m b) => ReadableBody (IxWriterT w m) b where
  readBody = ilift readBody

instance streamableBodyIxMonadWriterT :: (Monoid w, IxMonad m, StreamableBody m inner stream) => StreamableBody (IxWriterT w m) inner stream where
  streamBody = ilift <<< streamBody
