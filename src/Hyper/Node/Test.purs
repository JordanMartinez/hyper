module Hyper.Node.Test where

import Prelude

import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, modify_, runStateT)
import Data.Array (snoc)
import Data.Identity (Identity(..))
import Data.Indexed (Indexed(..))
import Data.Lens (_Just, over, set)
import Data.Tuple (Tuple)
import Hyper.Conn (type (&))
import Hyper.Request (class ReadableBody, class Request)
import Hyper.Response (class Response)
import Hyper.Test.Types (TestRequest(..), TestResponse, _responseBody, _responseHeaders, _status)
import Node.Buffer (Buffer)

newtype TestServer body m from to a =
  TestServer (Indexed (StateT (TestResponse body) (ReaderT TestRequest m)) from to a)

derive newtype instance functorTestServer :: Monad m => Functor (TestServer body m x x)
derive newtype instance applyTestServer :: Monad m => Apply (TestServer body m x x)
derive newtype instance applicativeTestServer :: Monad m => Applicative (TestServer body m x x)
derive newtype instance bindTestServer :: Monad m => Bind (TestServer body m x x)
derive newtype instance monadTestServer :: Monad m => Monad (TestServer body m x x)

derive newtype instance ixFunctorTestServer :: Monad m => IxFunctor (TestServer body m)
derive newtype instance ixApplyTestServer :: Monad m => IxApply (TestServer body m)
derive newtype instance ixApplicativeTestServer :: Monad m => IxApplicative (TestServer body m)
derive newtype instance ixBindTestServer :: Monad m => IxBind (TestServer body m)
derive newtype instance ixMonad :: Monad m => IxMonad (TestServer body m)

runTestServerPure :: forall fromReq fromRes toReq toRes body a
          . TestRequest
         -> TestResponse body
         -> TestServer body Identity (fromReq & fromRes) (toReq & toRes) a
         -> Tuple a (TestResponse body)
runTestServerPure request response server =
  let (Identity result) = runTestServer request response server
  in result

runTestServer :: forall m fromReq fromRes toReq toRes body a
          . Monad m
         => TestRequest
         -> TestResponse body
         -> TestServer body m (fromReq & fromRes) (toReq & toRes) a
         -> m (Tuple a (TestResponse body))
runTestServer request response (TestServer (Indexed server)) =
  runReaderT (runStateT server response) request

instance requestTestServer :: Monad m => Request (TestServer body m) where
    getRequestData = TestServer $ Indexed do
      TestRequest { url, headers, method } <- ask
      pure { url, headers, method }

    ignoreBody = TestServer (Indexed (pure unit))

instance readableBodyTestServerString :: Monad m => ReadableBody (TestServer String m) String where
  readBody = TestServer $ Indexed do
    TestRequest request <- ask
    pure request.body

instance responseTestServerArrayB :: Monad m => Response (TestServer (Array b) m) b where
  writeStatus status = TestServer $ Indexed do
    modify_ (set (_status <<< _Just) status)

  writeHeader header = TestServer $ Indexed do
    modify_ (over _responseHeaders (_ `snoc` header))

  closeHeaders = TestServer $ Indexed (pure unit)

  send chunk = TestServer $ Indexed do
    modify_ (over _responseBody (_ `snoc` chunk))

  end = TestServer $ Indexed (pure unit)
