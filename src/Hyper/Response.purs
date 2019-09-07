module Hyper.Response where

import Prelude

import Control.Monad.Indexed (class IxMonad)
import Control.Monad.Indexed.Qualified as Ix
import Control.ValidTransition (class ValidTransition)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
import Hyper.Conn (BodyOpen, Conn, HeadersOpen, ReqProxy, ResponseEnded, StatusLineOpen, kind RequestState, kind ResponseState)
import Hyper.Header (Header)
import Hyper.Status (Status, statusFound)

-- | The operations that a response writer, provided by the server backend,
-- | must support.
class (IxMonad m) <= Response m body | m -> body where
  writeStatus
    :: forall (reqState :: RequestState)
     . Status
    -> m (Conn reqState StatusLineOpen) (Conn reqState HeadersOpen) Unit

  writeHeader
    :: forall (reqState :: RequestState)
     . Header
    -> m (Conn reqState HeadersOpen) (Conn reqState HeadersOpen) Unit

  closeHeaders
    :: forall (reqState :: RequestState)
     . m (Conn reqState HeadersOpen) (Conn reqState BodyOpen) Unit

  send
    :: forall (reqState :: RequestState)
     . body
    -> m (Conn reqState BodyOpen) (Conn reqState BodyOpen) Unit

  end
    :: forall (reqState :: RequestState)
     . m (Conn reqState BodyOpen) (Conn reqState ResponseEnded) Unit

-- headers :: forall f m body (reqState :: RequestState)
--          . Foldable f
--         => ValidTransition (ReqProxy reqState) (ReqProxy reqState)
--         => Response m body
--         => f Header
--         -> m (Conn reqState HeadersOpen) (Conn reqState BodyOpen) Unit
-- headers hs = Ix.do
--   traverse_ writeHeader hs
--   closeHeaders

contentType :: forall m body (reqState :: RequestState)
             . ValidTransition (ReqProxy reqState) (ReqProxy reqState)
            => Response m body
            => MediaType
            -> m (Conn reqState HeadersOpen) (Conn reqState HeadersOpen) Unit
contentType mediaType =
  writeHeader (Tuple "Content-Type" (unwrap mediaType))

redirect :: forall m body (reqState :: RequestState)
          . ValidTransition (ReqProxy reqState) (ReqProxy reqState)
         => Response m body
         => String
         -> m (Conn reqState StatusLineOpen) (Conn reqState HeadersOpen) Unit
redirect uri = Ix.do
  writeStatus statusFound
  writeHeader (Tuple "Location" uri)

class (IxMonad m) <= ResponseWritable m responseData body where
  toResponse :: forall (reqState :: RequestState) (resState :: ResponseState)
              . responseData
             -> m (Conn reqState resState) (Conn reqState resState) body

respond :: forall m body r (reqState :: RequestState)
         . ValidTransition (ReqProxy reqState) (ReqProxy reqState)
        => Response m body
        => ResponseWritable m r body
        => r
        -> m (Conn reqState BodyOpen) (Conn reqState ResponseEnded) Unit
respond r = Ix.do
  response <- toResponse r
  send response
  end
