module Hyper.Node.Test where

import Prelude

import Control.Monad.Indexed (class IxApplicative, class IxApply, class IxBind, class IxFunctor, class IxMonad)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (StateT, modify_, runStateT)
import Data.Array (snoc)
import Data.Indexed (Indexed(..))
import Data.Lens (_Just, over, set)
import Data.Tuple (Tuple)
import Hyper.Conn (type (&))
import Hyper.Request (class ReadableBody, class Request)
import Hyper.Response (class Response)
import Hyper.Test.Types (TestRequest(..), TestResponse, _responseBody, _responseHeaders, _status)
import Node.Buffer (Buffer)

newtype TestServer body from to a =
  TestServer (Indexed (StateT (TestResponse body) (Reader TestRequest)) from to a)

derive newtype instance functorTestServer :: Functor (TestServer body x x)
derive newtype instance applyTestServer :: Apply (TestServer body x x)
derive newtype instance applicativeTestServer :: Applicative (TestServer body x x)
derive newtype instance bindTestServer :: Bind (TestServer body x x)
derive newtype instance monadTestServer :: Monad (TestServer body x x)

derive newtype instance ixFunctorTestServer :: IxFunctor (TestServer body)
derive newtype instance ixApplyTestServer :: IxApply (TestServer body)
derive newtype instance ixApplicativeTestServer :: IxApplicative (TestServer body)
derive newtype instance ixBindTestServer :: IxBind (TestServer body)
derive newtype instance ixMonad :: IxMonad (TestServer body)

runTestServer :: forall fromReq fromRes toReq toRes body a
          . TestServer body (fromReq & fromRes) (toReq & toRes) a
         -> TestRequest
         -> TestResponse body
         -> Tuple a (TestResponse body)
runTestServer (TestServer (Indexed server)) request response =
  runReader (runStateT server response) request

instance requestTestServer :: Request (TestServer body) where
    getRequestData = TestServer $ Indexed do
      TestRequest { url, headers, method } <- ask
      pure { url, headers, method }

    ignoreBody = TestServer (Indexed (pure unit))

instance readableBodyTestServerString :: ReadableBody (TestServer String) String where
  readBody = TestServer $ Indexed do
    TestRequest request <- ask
    pure request.body

instance responseTestServerArrayBuffer :: Response (TestServer (Array Buffer)) Buffer where
  writeStatus status = TestServer $ Indexed do
    modify_ (set (_status <<< _Just) status)

  writeHeader header = TestServer $ Indexed do
    modify_ (over _responseHeaders (_ `snoc` header))

  closeHeaders = TestServer $ Indexed (pure unit)

  send chunk = TestServer $ Indexed do
    modify_ (over _responseBody (_ `snoc` chunk))

  end = TestServer $ Indexed (pure unit)
else
instance responseTestServerArrayB :: Response (TestServer (Array b)) b where
  writeStatus status = TestServer $ Indexed do
    modify_ (set (_status <<< _Just) status)

  writeHeader header = TestServer $ Indexed do
    modify_ (over _responseHeaders (_ `snoc` header))

  closeHeaders = TestServer $ Indexed (pure unit)

  send chunk = TestServer $ Indexed do
    modify_ (over _responseBody (_ `snoc` chunk))

  end = TestServer $ Indexed (pure unit)
