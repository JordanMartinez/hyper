module Hyper.Conn where

-- | Defines the resStates of an HTTP request stream. It tracks whether or not
-- | some content has already been read from an HTTP request stream.
-- |
-- | Proper order of computations:
-- | BodyReadable -> BodyRead
foreign import kind RequestState

-- | Indicates that the request has been received but neither its
-- | headers nor body have been read yet.
foreign import data RequestReceived :: RequestState

-- | Indicatess the request's headers have been read, but
-- | it's body hasn't been read yet.
foreign import data HeadersRead :: RequestState

-- | Indicatess the request's body has already been read
-- | and can no longer be read again.
foreign import data BodyRead :: RequestState


-- | Defines the resStates of an HTTP response stream. It tracks whether or not
-- | some content has already been written to an HTTP response stream.
-- |
-- | Proper order of computations. Items marked with an asterisk indicate that
-- | transitioning back to the same resState is valid:
-- | StatusLineOpen -> HeadersOpen* -> BodyOpen* -> ResponseEnded
foreign import kind ResponseState

-- | Type indicating that the status-line is ready to be
-- | sent.
foreign import data StatusLineOpen :: ResponseState

-- | Type indicating that headers are ready to be
-- | sent, i.e. the body streaming has not been started.
foreign import data HeadersOpen :: ResponseState

-- | Type indicating that headers have already been
-- | sent, and that the body is currently streaming.
foreign import data BodyOpen :: ResponseState

-- | Type indicating that headers have already been
-- | sent, and that the body stream, and thus the response,
-- | is finished.
foreign import data ResponseEnded :: ResponseState

-- | A `Conn` models the entirety of an HTTP connection, containing the fields
-- | `request`, `response`, and the extensibility point `components`.
type Conn (request :: RequestState -> Type) (requestState :: RequestState)
          (response :: ResponseState -> Type) (responseState :: ResponseState)
          components =
  { request :: request requestState
  , response :: response responseState
  , components :: components
  }
