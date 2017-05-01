module Hyper.Drive.Response ( response
                            , status
                            , header
                            , body
                            ) where

import Prelude
import Data.StrMap as StrMap
import Data.MediaType (MediaType)
import Data.Tuple (Tuple(..))
import Hyper.Drive (Response)
import Hyper.Header (Header)
import Hyper.Status (Status, statusFound, statusOK)

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

redirect
  :: forall body
   . String
  -> Response body
  -> Response body
redirect location =
  status statusFound
  >>> header (Tuple "Location" location)

contentType
  :: forall body
   . MediaType
  -> Response body
  -> Response body
contentType mt =
  header (Tuple"Content-Type" (show mt))
