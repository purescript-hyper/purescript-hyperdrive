module Hyper.DriveSpec where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import Hyper.Drive (Response(..), hyperdrive)
import Hyper.Middleware (evalMiddleware)
import Hyper.Status (statusNotFound, statusOK)
import Hyper.Test.TestServer (TestRequest(..), TestResponse(..), defaultRequest, testHeaders, testServer, testStatus, testStringBody)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldContain, shouldEqual)

spec :: Spec Unit
spec = do
  let runHyperdrive app =
        { request: TestRequest $ defaultRequest { body = "Bonjour" }
        , response: TestResponse Nothing [] []
        , components: {}
        }
        # evalMiddleware (hyperdrive app)
        # testServer

  describe "Hyper.Drive" do
    describe "hyperdrive" do

      it "responds with the supplied status" do
        conn <- runHyperdrive (const $ pure $ Response { status: statusNotFound
                                                       , headers: mempty
                                                       , body: mempty
                                                       })
        testStatus conn `shouldEqual` Just statusNotFound

      it "responds with the supplied headers" do
        conn <- runHyperdrive (const $ pure $ Response { status: statusOK
                                                       , headers: Object.singleton "foo" "bar"
                                                       , body: mempty
                                                       })
        testHeaders conn `shouldContain` Tuple "foo" "bar"

      it "responds with the supplied body" do
        conn <- runHyperdrive (const $ pure $ Response { status: statusOK
                                                       , headers: mempty
                                                       , body: "Hello"
                                                       })
        testStringBody conn `shouldEqual` "Hello"

      it "knows the request body" do
        conn <- runHyperdrive (\req -> pure $ Response { status: statusOK
                                                       , headers: mempty
                                                       , body: (unwrap req).body
                                                       })
        testStringBody conn `shouldEqual` "Bonjour"
