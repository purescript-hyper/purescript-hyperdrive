module Hyper.Drive.Response ( redirect
                            , contentType
                            ) where

import Prelude
import Data.MediaType (MediaType)
import Data.Tuple (Tuple(..))
import Hyper.Drive (Response, header, status)
import Hyper.Status (statusFound)

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
