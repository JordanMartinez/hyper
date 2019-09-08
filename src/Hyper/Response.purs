module Hyper.Response where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed (class IxMonad)
import Data.MediaType (MediaType)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(Tuple))
import Hyper.Conn (Conn, BodyOpen, HeadersOpen, ResponseEnded, StatusLineOpen, kind ResponseState)
import Hyper.Header (Header)
import Hyper.Status (Status, statusFound)

-- | The operations that a response writer, provided by the server backend,
-- | must support.
class Response m body | m -> body where
  writeStatus
    :: forall reqState
     . Status
    -> m (Conn reqState StatusLineOpen) (Conn reqState HeadersOpen) Unit
  writeHeader
    :: forall reqState
     . Header
    -> m (Conn reqState HeadersOpen) (Conn reqState HeadersOpen) Unit
  closeHeaders
    :: forall reqState
     . m (Conn reqState HeadersOpen) (Conn reqState BodyOpen) Unit
  send
    :: forall reqState
     . body
    -> m (Conn reqState BodyOpen) (Conn reqState BodyOpen) Unit
  end
    :: forall reqState
     . m (Conn reqState BodyOpen) (Conn reqState ResponseEnded) Unit

-- headers
--   :: forall f m reqState body
--   .  Foldable f
--   => IxMonad m
--   => Response m body
--   => f Header
--   -> m (Conn reqState HeadersOpen) (Conn reqState BodyOpen) Unit
-- headers hs =
--   traverse_ writeHeader hs
--   :*> closeHeaders

contentType
  :: forall m reqState body
  .  IxMonad m
  => Response m body
  => MediaType
  -> m (Conn reqState HeadersOpen) (Conn reqState HeadersOpen) Unit
contentType mediaType =
  writeHeader (Tuple "Content-Type" (unwrap mediaType))

redirect
  :: forall m reqState body
  .  IxMonad m
  => Response m body
  => String
  -> m (Conn reqState StatusLineOpen) (Conn reqState HeadersOpen) Unit
redirect uri = Ix.do
  writeStatus statusFound
  writeHeader (Tuple "Location" uri)

class ResponseWritable m responseData body where
  toResponse :: forall i. responseData -> m i i body

respond
  :: forall m r body reqState
  .  IxMonad m
  => ResponseWritable m r body
  => Response m body
  => r
  -> m (Conn reqState BodyOpen) (Conn reqState ResponseEnded) Unit
respond r = Ix.do
  resp <- toResponse r
  send resp
  end
