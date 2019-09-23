module IndexedExample where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Control.Monad.Indexed.Reader.Class (class IxMonadAsk, iask)
import Control.Monad.Indexed.Reader.Trans (runIxReaderT)
import Control.Monad.Indexed.State.Class (class IxMonadState, iget)
import Control.Monad.Indexed.State.Trans (evalIxStateT)
import Effect (Effect)
import Effect.Console as Console
import Hyper.Conn (type (&), BodyRead, BodyUnread, ResponseEnded, StatusLineOpen)
import Hyper.Node.Server (Hostname(..), Port(..), runServer)
import Hyper.Request (class Request, ignoreBody)
import Hyper.Response (class Response, class ResponseWritable, closeHeaders, end, send, toResponse, writeStatus)
import Hyper.Status (statusOK)

main :: Effect Unit
main = runServer options (runIxReaderT (evalIxStateT middleware "foo") 4)
  where
    options = { hostname: Hostname "127.0.0.1"
              , port: Port 8080
              , onListening: \(Hostname h) (Port p) -> Console.log "Listening on http://127.0.0.1:8080"
              , onRequestError: \error -> Console.log $ "Error: " <> show error
              }

    middleware
      :: forall m body
       . IxMonadState String m
      => IxMonadAsk Int m
      => Request m
      => Response m body
      => ResponseWritable m String body
      => m (BodyUnread & StatusLineOpen) (BodyRead & ResponseEnded) Unit
    middleware = Ix.do
      ignoreBody
      writeStatus statusOK
      closeHeaders
      val <- iget
      bar <- iask
      resp <- toResponse (val <> show bar)
      send resp
      end
