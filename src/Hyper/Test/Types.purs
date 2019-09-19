module Hyper.Test.Types where

import Prelude

import Data.Either (Either(..))
import Data.HTTP.Method (CustomMethod, Method(..))
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Symbol (SProxy(..))
import Foreign.Object (Object, empty)
import Hyper.Header (Header)
import Hyper.Status (Status)

newtype TestRequest =
  TestRequest { url :: String
              , method :: Either Method CustomMethod
              , body :: String
              , headers :: Object String
              }
derive instance newtypeTestRequest :: Newtype TestRequest _

_url :: Lens' TestRequest String
_url = _Newtype <<< prop (SProxy :: SProxy "url")

_method :: Lens' TestRequest (Either Method CustomMethod)
_method = _Newtype <<< prop (SProxy :: SProxy "method")

_requestBody :: Lens' TestRequest String
_requestBody = _Newtype <<< prop (SProxy :: SProxy "body")

_requestHeaders :: Lens' TestRequest (Object String)
_requestHeaders = _Newtype <<< prop (SProxy :: SProxy "headers")

defaultRequest :: { url :: String
                  , method :: Either Method CustomMethod
                  , headers :: Object String
                  , body :: String
                  }
defaultRequest =
  { url: ""
  , method: Left GET
  , headers: empty
  , body: ""
  }

newtype TestResponse body
  = TestResponse { status :: Maybe Status
                 , headers :: Array Header
                 , body :: body
                 }
derive instance newtypeTestResponse :: Newtype (TestResponse body) _

_status :: forall b. Lens' (TestResponse b) (Maybe Status)
_status = _Newtype <<< prop (SProxy :: SProxy "status")

_responseHeaders :: forall b. Lens' (TestResponse b) (Array Header)
_responseHeaders = _Newtype <<< prop (SProxy :: SProxy "headers")

_responseBody :: forall b. Lens' (TestResponse b) b
_responseBody = _Newtype <<< prop (SProxy :: SProxy "body")
