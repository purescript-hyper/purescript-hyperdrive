module Hyper.Drive ( class Request
                   , requestHeaders
                   , requestBody
                   , response
                   , status
                   , header
                   , body
                   , Application
                   , hyperdrive
                   ) where

import Prelude
import Data.StrMap as StrMap
import Hyper.Request as Request
import Hyper.Response as Response
import Control.IxMonad (ibind)
import Data.StrMap (StrMap)
import Data.Tuple (Tuple(..), curry)
import Hyper.Conn (Conn)
import Hyper.Header (Header)
import Hyper.Middleware (Middleware, lift', runMiddleware)
import Hyper.Middleware.Class (getConn)
import Hyper.Status (Status, statusOK)

class Request r m body | r -> m, r -> body where
  requestHeaders :: r -> StrMap String
  requestBody :: r -> m body

data RequestWithHeaders req =
  RequestWithHeaders req (StrMap String)

instance requestRequest :: Request (RequestWithHeaders r) m body where
  requestHeaders (RequestWithHeaders _ hs) = hs
  requestBody (RequestWithHeaders r _) =
    runMiddleware (Request.readBody r)

newtype Response body =
  Response { status :: Status
           , headers :: StrMap String
           , body :: body
           }

type Application m req res = req -> m res

hyperdrive
  :: forall m hreq hres req res
   . Monad m
  => Request req m
  => Response.Response res m r
  => Response.ResponseWritable r m resBody
  => Application m req res
  -> Middleware
     m
     (Conn hreq (hres Response.StatusLineOpen) components)
     (Conn hreq (hres Response.ResponseEnded) components)
     Unit
hyperdrive app = do
  conn <- getConn
  { headers } <- Request.getRequestData
  components <- _.components <$> getConn
  let req = RequestWithHeaders conn.request headers
  res <- lift' (app req)
  Response.writeStatus res.status
  StrMap.foldM (const (curry Response.writeHeader)) unit res.headers
  Response.closeHeaders
  Response.toResponse res.body >>= Response.send
  Response.end
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
