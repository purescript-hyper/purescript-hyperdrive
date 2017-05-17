module Hyper.Drive ( Request(..)
                   , Response(..)
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

newtype Request body components =
  Request { headers :: StrMap String
          , body :: body
          , components :: components
          }

newtype Response body =
  Response { status :: Status
           , headers :: StrMap String
           , body :: body
           }

type Application m req res = req -> m res

hyperdrive
  :: forall m req res r components resBody
   . Monad m
  => Request req m
  => Response res m r
  => ResponseWritable r m resBody
  => Application m (Request Unit components) (Response resBody)
  -> Middleware
     m
     (Conn req (res StatusLineOpen) components)
     (Conn req (res ResponseEnded) components)
     Unit
hyperdrive app = do
  { headers } <- getRequestData
  components <- _.components <$> getConn
  let req = Request { headers: headers
                    , body: unit
                    , components: components
                    }
  Response res <- lift' (app req)
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
  Response
  { status: statusOK
  , headers: StrMap.empty
  , body: b
  }

status
  :: forall body
   . Status
  -> Response body
  -> Response body
status status' (Response res) =
  Response (res { status = status' })

header
  :: forall body
   . Header
  -> Response body
  -> Response body
header (Tuple k v) (Response res) =
  Response (res { headers = StrMap.insert k v res.headers })

body
  :: forall a body
   . body
  -> Response a
  -> Response body
body b (Response res) =
  Response (res { body = b })
