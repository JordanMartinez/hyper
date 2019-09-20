module Hyper.Session
       ( SessionID(..)
       , class SessionStore
       , newSessionID
       , get
       , put
       , delete
       , saveSession
       , getSession
       , deleteSession
       ) where

import Prelude

import Control.Monad.Indexed (class IxMonad, ipure, (:>>=))
import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed.Reader.Class (class IxMonadAsk, iasks)
import Data.Either (Either(..))
import Data.Maybe (Maybe(Nothing, Just), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.NonEmpty as NonEmpty
import Foreign.Object as Object
import Hyper.Conn (type (&), HeadersOpen, kind RequestState, kind ResponseState)
import Hyper.Cookies (SameSite(Lax), cookies, defaultCookieAttributes, maxAge, setCookie)
import Hyper.Request (class Request)
import Hyper.Response (class Response)
import Type.Row (type (+))

newtype SessionID = SessionID String

derive instance eqSessionID :: Eq SessionID
derive instance ordSessionID :: Ord SessionID
derive instance newtypeSessionID :: Newtype SessionID _

class IxMonad m <= SessionStore store m session | store -> m, store -> session where
  newSessionID :: forall x. store -> m x x SessionID
  get :: forall x. store -> SessionID -> m x x (Maybe session)
  put :: forall x. store -> SessionID -> session -> m x x Unit
  delete :: forall x. store -> SessionID -> m x x Unit

type Sessions s = { key :: String, store :: s }

type SESSION_ROWS store r = ( sessions :: Sessions store | r)

currentSessionID
  :: forall m req (res :: ResponseState) r store session
  .  IxMonadAsk { | SESSION_ROWS store + r } m
  => Request m
  => SessionStore store m session
  => m (req & res) (req & res) (Maybe SessionID)
currentSessionID = Ix.do
  c <- cookies
  case c  of
    Left err ->
      ipure Nothing
    Right cookies -> Ix.do
      s <- iasks _.sessions
      ipure ((SessionID <<< NonEmpty.head) <$> Object.lookup s.key cookies)

getSession
  :: forall m (req :: RequestState) (res :: ResponseState) r store session
  .  IxMonadAsk { | SESSION_ROWS store + r } m
  => Request m
  => SessionStore store m session
  => m (req & res) (req & res) (Maybe session)
      -- { | SESSION_ROWS store + COOKIES_ROWS' c }
      -- { | SESSION_ROWS store + COOKIES_ROWS' c }
      -- (Maybe session)
getSession = Ix.do
  sessionId <- currentSessionID
  case sessionId of
    Just id' -> Ix.do
      s <- iasks _.sessions
      get s.store id'
    Nothing -> ipure Nothing

saveSession
  :: forall m (req :: RequestState) r b store session
  .  IxMonadAsk { | SESSION_ROWS store + r } m
  => Request m
  => Response m b
  => SessionStore store m session
  => session
  -> m (req & HeadersOpen) (req & HeadersOpen) Unit
      -- { | SESSION_ROWS store + COOKIES_ROWS' c }
      -- { | SESSION_ROWS store + COOKIES_ROWS' c }
      -- Unit
saveSession session = Ix.do
  s <- iasks _.sessions
  sessionId <-
    currentSessionID :>>= case _ of
      Just id'
        | unwrap id' /= "" -> ipure id'
        | otherwise -> newSessionID s.store
      Nothing -> newSessionID s.store
  put s.store sessionId session
  setCookie
    s.key
    (unwrap sessionId)
    (defaultCookieAttributes { sameSite=Just Lax, httpOnly=true })

deleteSession
  :: forall m (req :: RequestState) r b store session
  .  IxMonadAsk { | SESSION_ROWS store + r } m
  => Request m
  => Response m b
  => SessionStore store m session
  => m (req & HeadersOpen) (req & HeadersOpen) Unit
      -- { | SESSION_ROWS store + COOKIES_ROWS' c }
      -- { | SESSION_ROWS store + COOKIES_ROWS' c }
      -- Unit
deleteSession = Ix.do
  sessionId <- currentSessionID
  s <- iasks _.sessions
  maybe (ipure unit) (delete s.store) sessionId
  -- TODO: Better delete?
  setCookie s.key "" (defaultCookieAttributes { maxAge=maxAge 0 })
