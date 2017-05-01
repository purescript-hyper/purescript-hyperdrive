module Hyper.Drive ( Request
                   , Response
                   , Application
                   , hyperdrive
                   , response
                   , status
                   , header
                   , body
                   ) where

import Prelude
import Data.StrMap as StrMap
import Control.IxMonad (ibind)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..), curry)
import Hyper.Conn (Conn)
import Hyper.Header (Header)
import Hyper.Middleware (Middleware, lift')
import Hyper.Middleware.Class (getConn)
import Hyper.Request (class Request, getRequestData)
import Hyper.Response (class Response, class ResponseWritable, ResponseEnded, StatusLineOpen, closeHeaders, end, send, toResponse, writeHeader, writeStatus)
import Hyper.Status (Status, statusOK)

type Request components =
  { headers :: StrMap String
  , components :: components
  }

type Response body =
  { status :: Status
  , headers :: StrMap String
  , body :: body
  }

type Application m components body =
  Request components -> m (Response body)

hyperdrive
  :: forall m req res components body r
   . Monad m
  => Request req m
  => Response res m body
  => ResponseWritable body m r
  => Application m components r
  -> Middleware
     m
     (Conn req (res StatusLineOpen) components)
     (Conn req (res ResponseEnded) components)
     Unit
hyperdrive app = do
  { headers } <- getRequestData
  components <- _.components <$> getConn
  res <- lift' (app { headers: headers
                    , components: components
                    })
  writeStatus res.status
  StrMap.foldM (const (curry writeHeader)) unit res.headers
  closeHeaders
  toResponse res.body >>= send
  end
  where
    bind = ibind
    discard = ibind

response
  :: forall body
   .  body
   -> Response body
response b =
  { status: statusOK
  , headers: StrMap.empty
  , body: b
  }

status
  :: forall body
   . Status
  -> Response body
  -> Response body
status status' res =
  res { status = status' }

header
  :: forall body
   . Header
  -> Response body
  -> Response body
header (Tuple k v) res =
  res { headers = StrMap.insert k v res.headers }

body
  :: forall a body
   . body
  -> Response a
  -> Response body
body b res =
  res { body = b }
