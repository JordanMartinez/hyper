module Hyper.Request where

import Prelude

import Control.Monad.Indexed (class IxMonad)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Lazy (Lazy)
import Data.Maybe (Maybe, fromMaybe)
import Data.String as String
import Data.Tuple (Tuple)
import Foreign.Object (Object)
import Hyper.Conn (BodyRead, BodyUnread, Conn, kind RequestState, kind ResponseState)
import Hyper.Form.Urlencoded (parseUrlencoded)

type RequestData =
  { url :: String
  , parsedUrl :: Lazy ParsedUrl
  , contentLength :: Maybe Int
  , headers :: Object String
  , method :: Either Method CustomMethod
  }

type ParsedUrl =
  { path :: Array String
  , query :: Either String (Array (Tuple String (Maybe String)))
  }

parseUrl :: String -> ParsedUrl
parseUrl url =
  let
    idx = fromMaybe (String.length url) $ String.indexOf (String.Pattern "?") url
    rawPath = String.take idx url
    rawQuery = String.drop (idx + 1) url
    path = Array.filter (_ /= "") $ String.split (String.Pattern "/") rawPath
    query = lmap (const rawQuery) $ parseUrlencoded rawQuery
  in
    {path, query}

class IxMonad m <= Request m where
  getRequestData
    :: forall (reqState :: RequestState) (resState :: ResponseState)
     . m (Conn reqState resState) (Conn reqState resState) RequestData

  ignoreBody
    :: forall (resState :: ResponseState)
     . m (Conn BodyUnread resState) (Conn BodyRead resState) Unit

-- | A `ReadableBody` instance reads the complete request body as a
-- | value of type `b`. For streaming the request body, see the
-- | [StreamableBody](#streamablebody) class.
class Request m <= ReadableBody m body | m -> body where
  readBody
    :: forall (resState :: ResponseState)
     . m (Conn BodyUnread resState) (Conn BodyRead resState) body

-- | A `StreamableBody` instance returns a stream of the request body,
-- | of type `stream`. To read the whole body as a value, without
-- | streaming, see the [ReadableBody](#readablebody) class.
class (Request m, Monad innerMonad) <= StreamableBody m innerMonad stream | m -> innerMonad stream where
  streamBody
    :: forall (resState :: ResponseState)
     . (stream -> innerMonad Unit)
     -> m (Conn BodyUnread resState) (Conn BodyRead resState) Unit
