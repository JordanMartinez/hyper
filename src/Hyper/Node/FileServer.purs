module Hyper.Node.FileServer (fileServer) where

import Prelude

import Control.Monad.Indexed (class IxMonad, (:>>=))
import Control.Monad.Indexed.Qualified as Ix
import Data.Array (last)
import Data.Map (Map, fromFoldable, lookup)
import Data.Maybe (maybe)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(Tuple))
import Effect.Aff.Indexed.Class (class IxMonadAff, iliftAff)
import Effect.Indexed.Class (iliftEffect)
import Hyper.Conn (type (&), ResponseEnded, StatusLineOpen, kind RequestState, kind ResponseState)
import Hyper.Request (class Request, getRequestData)
import Hyper.Response (class Response, class ResponseWritable, closeHeaders, end, send, toResponse, writeHeader, writeStatus)
import Hyper.Status (statusOK)
import Node.Buffer (Buffer)
import Node.Buffer as Buffer
import Node.FS.Aff (readFile, stat, exists)
import Node.FS.Stats (isDirectory, isFile)
import Node.Path (FilePath)
import Node.Path as Path

htaccess :: Map String String
htaccess = fromFoldable $
  [ Tuple "aab" "application/x-authorware-bin"
  , Tuple "aam" "application/x-authorware-map"
  , Tuple "aas" "application/x-authorware-seg"
  , Tuple "asc" "text/plain"
  , Tuple "asf" "video/x-ms-asf"
  , Tuple "asp" "text/html"
  , Tuple "asx" "video/x-ms-asf"
  , Tuple "avi" "application/octet-stream"
  , Tuple "awk" "text/plain"
  , Tuple "bash" "text/plain"
  , Tuple "bsh" "text/plain"
  , Tuple "bz2" "application/octet-stream"
  , Tuple "c" "text/plain"
  , Tuple "cgi" "text/plain"
  , Tuple "chm" "application/octet-stream"
  , Tuple "class" "application/x-java-applet"
  , Tuple "csh" "text/plain"
  , Tuple "css" "text/css"
  , Tuple "csv" "application/vnd.ms-excel"
  , Tuple "dcr" "application/x-director"
  , Tuple "dir" "application/x-director"
  , Tuple "dmg" "application/octet-stream"
  , Tuple "dxr" "application/x-director"
  , Tuple "exe" "application/octet-stream"
  , Tuple "fgd" "application/x-director"
  , Tuple "fh" "image/x-freehand"
  , Tuple "fh4" "image/x-freehand"
  , Tuple "fh5" "image/x-freehand"
  , Tuple "fh7" "image/x-freehand"
  , Tuple "fhc" "image/x-freehand"
  , Tuple "flv" "video/x-flv"
  , Tuple "gawk" "text/plain"
  , Tuple "gtar" "application/x-gtar"
  , Tuple "gz" "application/x-gzip"
  , Tuple "h" "text/plain"
  , Tuple "ico" "image/vnd.microsoft.icon"
  , Tuple "in" "text/plain"
  , Tuple "ini" "text/plain"
  , Tuple "m3u" "audio/x-mpegurl"
  , Tuple "md5" "text/plain"
  , Tuple "mov" "application/octet-stream"
  , Tuple "mov" "video/quicktime"
  , Tuple "mp4" "application/octet-stream"
  , Tuple "mpg" "application/octet-stream"
  , Tuple "msi" "application/octet-stream"
  , Tuple "nawk" "text/plain"
  , Tuple "pdb" "application/x-pilot"
  , Tuple "pdf" "application/pdf"
  , Tuple "phps" "application/x-httpd-php-source"
  , Tuple "pl" "text/plain"
  , Tuple "prc" "application/x-pilot"
  , Tuple "py" "text/plain"
  , Tuple "qt" "video/quicktime"
  , Tuple "ra" "audio/vnd.rn-realaudio"
  , Tuple "ram" "audio/vnd.rn-realaudio"
  , Tuple "rar" "application/x-rar-compressed"
  , Tuple "rm" "application/vnd.rn-realmedia"
  , Tuple "rpm" "audio/x-pn-realaudio-plugin"
  , Tuple "rv" "video/vnd.rn-realvideo"
  , Tuple "sh" "text/plain"
  , Tuple "sha" "text/plain"
  , Tuple "sha1" "text/plain"
  , Tuple "shtml" "text/html"
  , Tuple "svg" "image/svg+xml"
  , Tuple "svgz" "image/svg+xml"
  , Tuple "swf" "application/x-shockwave-flash"
  , Tuple "tgz" "application/octet-stream"
  , Tuple "torrent" "application/x-bittorrent"
  , Tuple "var" "text/plain"
  , Tuple "wav" "audio/x-wav"
  , Tuple "wax" "audio/x-ms-wax"
  , Tuple "wm" "video/x-ms-wm"
  , Tuple "wma" "audio/x-ms-wma"
  , Tuple "wmd" "application/x-ms-wmd"
  , Tuple "wmv" "video/x-ms-wmv"
  , Tuple "wmx" "video/x-ms-wmx"
  , Tuple "wmz" "application/x-ms-wmz"
  , Tuple "wvx" "video/x-ms-wvx"
  , Tuple "xbm" "image/x-xbitmap"
  , Tuple "xhtml" "application/xhtml+xml"
  , Tuple "xls" "application/octet-stream"
  , Tuple "xml" "text/xml"
  , Tuple "xrdf" "application/xrds+xml"
  , Tuple "zip" "application/zip"
  ]

serveFile
  :: forall m (req :: RequestState) b
  .  IxMonad m
  => IxMonadAff m
  => ResponseWritable m Buffer b
  => Response m b
  => FilePath
  -> m (req & StatusLineOpen) (req & ResponseEnded) Unit
serveFile = serveFile' htaccess

serveFile'
  :: forall m (req :: RequestState) b
  .  IxMonad m
  => IxMonadAff m
  => ResponseWritable m Buffer b
  => Response m b
  => Map String String
  -> FilePath
  -> m (req & StatusLineOpen) (req & ResponseEnded) Unit
serveFile' htaccessMap path = Ix.do
  let
    ext = last $ split (Pattern ".") path
    contentType = maybe "*/*" identity (ext >>= flip lookup htaccessMap)
  buf <- iliftAff (readFile path)
  contentLength <- iliftEffect (Buffer.size buf)
  writeStatus statusOK
  writeHeader $ Tuple "Content-Type" (contentType <> "; charset=utf-8")
  writeHeader $ Tuple "Content-Length" (show contentLength)
  closeHeaders
  response <- toResponse buf
  send response
  end

fileServer
  :: forall m (req :: RequestState) b
  .  IxMonad m
  => IxMonadAff m
  => Request m
  => ResponseWritable m Buffer b
  => Response m b
  => FilePath
  -> m (req & StatusLineOpen) (req & ResponseEnded) Unit
  -> m (req & StatusLineOpen) (req & ResponseEnded) Unit
fileServer = fileServer' htaccess

-- | Extremly basic implementation of static file serving. Needs more love.
fileServer'
  :: forall m (req :: RequestState) b
  .  IxMonad m
  => IxMonadAff m
  => Request m
  => ResponseWritable m Buffer b
  => Response m b
  => Map String String
  -> FilePath
  -> m (req & StatusLineOpen) (req & ResponseEnded) Unit
  -> m (req & StatusLineOpen) (req & ResponseEnded) Unit
fileServer' htaccessMap dir on404 = Ix.do
  { url } <- getRequestData
  serve (Path.concat [dir, url])
  where
    serveStats absolutePath stats
      | isFile stats = serveFile' htaccessMap absolutePath
      | isDirectory stats = serve (Path.concat [absolutePath, "index.html"])
      | otherwise = on404

    serve absolutePath = Ix.do
      fExists ← iliftAff (exists absolutePath)
      if fExists
        then iliftAff (stat absolutePath) :>>= serveStats absolutePath
        else on404
