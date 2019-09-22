module Hyper.CookiesSpec where

import Prelude

import Control.Alternative (empty)
import Control.Monad.Indexed.Qualified as Ix
import Data.Array ((:))
import Data.Either (Either(..), isLeft)
import Data.JSDate (jsdate)
import Data.Lens (view)
import Data.Lens.Lens.Tuple (_2)
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, fromNonEmpty, (:|))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import Effect.Aff (Aff)
import Foreign.Object (Object)
import Foreign.Object as Object
import Hyper.Conn (type (&), BodyRead, HeadersOpen)
import Hyper.Cookies (SameSite(..), cookies, defaultCookieAttributes, maxAge, setCookie)
import Hyper.Node.Test (TestServer, runTestServer)
import Hyper.Request (ignoreBody)
import Hyper.Response (closeHeaders, end, writeStatus)
import Hyper.Status (statusOK)
import Hyper.Test.Types (TestRequest(..), _responseHeaders, defaultRequest, emptyResponse)
import Test.Spec (it, Spec, describe)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do

  describe "Hyper.Node.Cookies" do

    describe "cookies" do

      it "parses a no cookies" do
        c <- parseCookies ""
        c `shouldEqual` Right Object.empty

      it "parses a single cookie" do
        c <- parseCookies "foo=1"
        c `shouldEqual` Right (Object.singleton "foo" ("1" :| empty))

      it "parses multiple cookies" do
        c <- parseCookies "foo=1;bar=2;baz=3"
        c `shouldEqual`
          Right (Object.fromFoldable
                 [ Tuple "foo" ("1" :| empty)
                 , Tuple "bar" ("2" :| empty)
                 , Tuple "baz" ("3" :| empty)
                 ])

      it "parses multiple cookie values for same key" do
        cookies <- parseCookies "foo=1;bar=2;foo=3"
        let values = map (cookieValues "foo") cookies
        values `shouldEqual` Right (Just (Set.fromFoldable ["1", "3"]))

      it "ignores blanks" do
        c <- parseCookies "     ;  ;foo=3; ; ; ;;;"
        c `shouldEqual` Right (Object.singleton "foo" ("3" :| empty))

      it "fails on invalid pairs" do
        c <- parseCookies "foo"
        isLeft c `shouldEqual` true
    --
      it "fails on triples" do
        c <- parseCookies "foo=bar=baz"
        isLeft c `shouldEqual` true

    describe "setCookie" do

      it "sets a simple cookie" do
        headers <- writeCookies $ setCookie "foo" "bar" defaultCookieAttributes
        headers `shouldEqual` [Tuple "Set-Cookie" "foo=bar"]

      it "sets cookie with attributes" do
        let
          expires =
            jsdate
              { year : 2017.0
              , month : 7.0
              , day : 4.0
              , hour : 0.0
              , minute : 40.0
              , second : 0.0
              , millisecond : 0.0
              }
          attrs =
            { comment: Just "comment"
            , domain: Just "localhost"
            , expires: Just expires
            , httpOnly : true
            , maxAge: maxAge 3600
            , path : Just "/path"
            , sameSite : Just Strict
            , secure : true
            }
        headers <- writeCookies $ setCookie "foo" "bar" attrs
        headers `shouldEqual`
          [(Tuple
            "Set-Cookie"
            "foo=bar;HttpOnly;Secure;Comment=comment;Expires=Fri, 04 Aug 2017 00:40:00 GMT;Max-Age=3600;Domain=localhost;Path=/path;SameSite=Strict")]

      it "URL encodes cookie key" do
        headers <- writeCookies $ setCookie "&stuff!we like" "bar" defaultCookieAttributes
        headers `shouldEqual` [Tuple "Set-Cookie" "%26stuff!we%20like=bar"]

      it "URL encodes cookie value" do
        headers <- writeCookies $ setCookie "yeah" "=& ?%" defaultCookieAttributes
        headers `shouldEqual` [Tuple "Set-Cookie" "yeah=%3D%26%20%3F%25"]

  where
    parseCookies :: String -> Aff (Either String (Object (NonEmpty Array String)))
    parseCookies s = fst <$> runTestServer req emptyResponse server
      where
        req = TestRequest (defaultRequest { headers = Object.singleton "cookie" s })

        server = Ix.do
          ignoreBody
          writeStatus statusOK
          closeHeaders
          end
          cookies

    parseMultipleCookies :: String -> Aff (Either String (Object (NonEmpty Array String)))
    parseMultipleCookies s = fst <$> runTestServer req emptyResponse server
      where
        req = TestRequest (defaultRequest { headers = Object.singleton "cookie" s })

        server = Ix.do
          ignoreBody
          writeStatus statusOK
          closeHeaders
          end
          cookies

    cookieValues :: String -> Object (NonEmpty Array String) -> Maybe (Set String)
    cookieValues key =
      Object.lookup key
      >>> map (fromNonEmpty (:))
      >>> map Set.fromFoldable

    writeCookies :: forall a
                  . TestServer (Array a) Aff (BodyRead & HeadersOpen) (BodyRead & HeadersOpen) Unit
                 -> Aff (Array (Tuple String String))
    writeCookies setCookies = do
      result <- runTestServer (TestRequest defaultRequest) emptyResponse Ix.do
        ignoreBody
        writeStatus statusOK
        setCookies
        closeHeaders
        end
      pure $ view (_2 <<< _responseHeaders) result
