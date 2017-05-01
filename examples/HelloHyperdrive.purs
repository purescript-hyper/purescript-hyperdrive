module Examples.HelloHyperdrive where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Tuple (Tuple(..))
import Hyper.Drive (Application, Request, Response, header, hyperdrive, response, status)
import Hyper.Node.Server (defaultOptionsWithLogging, runServer)
import Hyper.Status (statusOK)
import Node.HTTP (HTTP)

app
  :: forall m body
   . Applicative m
  => Application m (Request body {}) (Response String)
app _ =
  response "Hello Hyperdrive!"
  # status statusOK
  # header (Tuple "X-Hello" "Hyperdrive")
  # pure

main :: forall e. Eff (console :: CONSOLE, http :: HTTP | e) Unit
main = runServer defaultOptionsWithLogging {} (hyperdrive app)
