module Examples.StateT where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.State (evalStateT, modify, modify_)
import Control.Monad.State.Trans (StateT)
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff)
import Hyper.Middleware (lift')
import Hyper.Node.Server (defaultOptionsWithLogging, runServer')
import Hyper.Response (closeHeaders, respond, writeStatus)
import Hyper.Status (statusOK)


runAppM ∷ ∀ a. StateT (Array String) Aff a → Aff a
runAppM = flip evalStateT []


main :: Effect Unit
main =
  let
      -- Our application just appends to the state in between
      -- some operations, then responds with the built up state...
      app = Ix.do
        lift' (modify_ (flip append ["I"]))
        writeStatus statusOK

        lift' (modify_ (flip append ["have"]))
        closeHeaders

        msgs <- lift' (modify (flip append ["state."]))
        respond (joinWith " " msgs)

  in runServer' defaultOptionsWithLogging {} runAppM app
