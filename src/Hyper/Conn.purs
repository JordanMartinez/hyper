module Hyper.Conn where

import Control.ValidTransition (class ValidTransition)

-- | Defines the state of an HTTP request stream. It tracks whether or not
-- | some content has already been read from an HTTP request stream.
-- |
-- | Proper order of computations:
-- | BodyUnread -> BodyRead
foreign import kind RequestState

-- | Indicatess the request's body hasn't been read yet.
foreign import data BodyUnread :: RequestState

-- | Indicatess the request's body has already been read
-- | and can no longer be read again.
foreign import data BodyRead :: RequestState

data ReqProxy (state :: RequestState)

instance validTransitionUnreadToUnread :: ValidTransition (ReqProxy BodyUnread) (ReqProxy BodyUnread)
else
instance validTransitionReadToRead :: ValidTransition (ReqProxy BodyRead) (ReqProxy BodyRead)
else
instance validTransitionUnreadToRead :: ValidTransition (ReqProxy BodyUnread) (ReqProxy BodyRead)

-- | Defines the state of an HTTP response stream. It tracks whether or not
-- | some content has already been written to an HTTP response stream.
-- |
-- | Proper order of computations. Items marked with an asterisk indicate that
-- | transitioning back to the same state is valid:
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

data ResProxy (state :: ResponseState)

instance validTransitionStatusLineOpenToSelf :: ValidTransition (ResProxy StatusLineOpen) (ResProxy StatusLineOpen)
else
instance validTransitionStatusStatusLineOpenToHeadersOpen :: ValidTransition (ResProxy StatusLineOpen) (ResProxy HeadersOpen)
else
instance validTransitionStatusHeadersOpenToSelf :: ValidTransition (ResProxy HeadersOpen) (ResProxy HeadersOpen)
else
instance validTransitionStatusHeadersOpenToBodyOpen :: ValidTransition (ResProxy HeadersOpen) (ResProxy BodyOpen)
else
instance validTransitionStatusBodyOpenToSelf :: ValidTransition (ResProxy BodyOpen) (ResProxy BodyOpen)
else
instance validTransitionStatusBodyOpenToResponseEnded :: ValidTransition (ResProxy BodyOpen) (ResProxy ResponseEnded)

data Conn (requestState :: RequestState) (responseState :: ResponseState)

instance validTransitionConn
  :: ( ValidTransition (ReqProxy fromReq) (ReqProxy toReq)
     , ValidTransition (ResProxy fromRes) (ResProxy toRes)
     ) => ValidTransition (Conn fromReq fromRes) (Conn toReq toRes)


-- -- | A `Conn` models the entirety of an HTTP connection, containing the fields
-- -- | `request`, `response`, and the extensibility point `components`.
-- type Conn (request :: RequestState -> Type) (requestState :: RequestState)
--           (response :: ResponseState -> Type) (responseState :: ResponseState)
--           components =
--   { request :: request requestState
--   , response :: response responseState
--   , components :: components
--   }
--
-- -- | Alias for easily defining both the request and response state transitions,
-- -- | excluding a change in the component type (to do that, use
-- -- | `ConnTransition'` instead)
-- type ConnTransition
--       m
--       (request :: RequestState -> Type)
--       (fromRequestState :: RequestState)
--       (toRequestState :: RequestState)
--       (response :: ResponseState -> Type)
--       (fromResponseState :: ResponseState)
--       (toResponseState :: ResponseState)
--       comp
--       a
--   = ConnTransition'
--         m
--         request
--         fromRequestState
--         toRequestState
--         response
--         fromResponseState
--         toResponseState
--         comp
--         comp
--         a
--
-- -- | Alias for easily defining both the request and response state transitions,
-- -- | including a change in the component type.
-- type ConnTransition'
--       m
--       (request :: RequestState -> Type)
--       (fromRequestState :: RequestState)
--       (toRequestState :: RequestState)
--       (response :: ResponseState -> Type)
--       (fromResponseState :: ResponseState)
--       (toResponseState :: ResponseState)
--       fromComp
--       toComp
--       a
--   = Middleware
--       m
--       (Conn request fromRequestState response fromResponseState fromComp)
--       (Conn request toRequestState   response toResponseState   toComp)
--       a
--
-- -- | Defines a Request state transition,
-- -- | excluding a change in the component type.
-- type RequestTransition
--       m
--       (request :: RequestState -> Type)
--       (fromRequestState :: RequestState)
--       (toRequestState :: RequestState)
--       (response :: ResponseState -> Type)
--       (responseState :: ResponseState)
--       comp
--       a
--   = RequestTransition'
--       m
--       request
--       fromRequestState
--       toRequestState
--       response
--       responseState
--       comp
--       comp
--       a
--
-- -- | Defines a Request state transition,
-- -- | including a change in the component type.
-- type RequestTransition'
--       m
--       (request :: RequestState -> Type)
--       (fromRequestState :: RequestState)
--       (toRequestState :: RequestState)
--       (response :: ResponseState -> Type)
--       (responseState :: ResponseState)
--       fromComp
--       toComp
--       a
--   = ConnTransition'
--       m
--       request
--       fromRequestState
--       toRequestState
--       response
--       responseState
--       responseState
--       fromComp
--       toComp
--       a
--
-- -- | Defines a Response state transition,
-- -- | excluding a change in the component type.
-- type ResponseTransition
--       m
--       (request :: RequestState -> Type)
--       (requestState :: RequestState)
--       (response :: ResponseState -> Type)
--       (fromResponseState :: ResponseState)
--       (toResponseState :: ResponseState)
--       comp
--       a
--   = ResponseTransition'
--       m
--       request
--       requestState
--       response
--       fromResponseState
--       toResponseState
--       comp
--       comp
--       a
--
-- -- | Defines a Response state transition,
-- -- | including a change in the component type.
-- type ResponseTransition'
--       m
--       (request :: RequestState -> Type)
--       (requestState :: RequestState)
--       (response :: ResponseState -> Type)
--       (fromResponseState :: ResponseState)
--       (toResponseState :: ResponseState)
--       fromComp
--       toComp
--       a
--   = ConnTransition'
--       m
--       request
--       requestState
--       requestState
--       response
--       fromResponseState
--       toResponseState
--       fromComp
--       toComp
--       a
--
-- -- | Indicates that no state transition occurs in either the request
-- -- | or the response. Moreover, the component type does not change.
-- type NoTransition
--       m
--       (request :: RequestState -> Type)
--       (requestState :: RequestState)
--       (response :: ResponseState -> Type)
--       (responseState :: ResponseState)
--       comp
--       a
--   = NoTransition'
--       m
--       request
--       requestState
--       response
--       responseState
--       comp
--       comp
--       a
--
-- -- | Indicates that no state transition occurs in either the request
-- -- | or the response. However, the component type does change.
-- type NoTransition'
--       m
--       (request :: RequestState -> Type)
--       (requestState :: RequestState)
--       (response :: ResponseState -> Type)
--       (responseState :: ResponseState)
--       fromComp
--       toComp
--       a
--   = ConnTransition'
--       m
--       request
--       requestState
--       requestState
--       response
--       responseState
--       responseState
--       fromComp
--       toComp
--       a
