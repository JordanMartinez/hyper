module Hyper.Node.Session.InMemory where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed (class IxMonad, ipure)
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Console (log)
import Effect.Indexed.Class (class IxMonadEffect, iliftEffect)
import Effect.Ref (Ref, modify_, new, read)
import Hyper.Session (class SessionStore, SessionID(..))

newtype InMemorySessionStore session = InMemorySessionStore (Ref (Map SessionID session))

foreign import generatedSessionID :: Effect String

instance sessionStoreInMemorySessionStore :: ( IxMonad m
                                             , IxMonadEffect m
                                             )
                                          => SessionStore
                                            (InMemorySessionStore session)
                                            m
                                            session where
  newSessionID _ = Ix.do
    id <- iliftEffect generatedSessionID
    ipure (SessionID id)

  get (InMemorySessionStore var) id =
    iliftEffect do
      log ("Looking up session: " <> show (unwrap id))
      Map.lookup id <$> read var

  put (InMemorySessionStore var) id session =
    iliftEffect do
      log ("Saving session: " <> unwrap id)
      flip modify_ var $ Map.insert id session

  delete (InMemorySessionStore var) id =
    iliftEffect do
      log ("Deleting session: " <> unwrap id)
      flip modify_ var $ Map.delete id

newInMemorySessionStore
  :: forall session
   . Effect (InMemorySessionStore session)
newInMemorySessionStore = InMemorySessionStore <$> new Map.empty
