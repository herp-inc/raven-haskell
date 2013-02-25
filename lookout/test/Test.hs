import Test.Hspec
import Control.Exception (evaluate)

import qualified System.Log.Lookout.Types as LT

dsn = "http://public_key:secret_key@example.com/sentry/project-id"
ss = LT.SentrySettings {
         LT.sentryURI = "http://example.com/sentry/",
         LT.sentryPublicKey = "public_key",
         LT.sentryPrivateKey = "secret_key",
         LT.sentryProjectId = "project-id"
     }

main :: IO ()
main = hspec $ do
  describe "Types" $ do
    it "parses empty DSN into SentryDisabled" $ do
        LT.fromDSN "" `shouldBe` LT.SentryDisabled

    it "carps on invalid DSN" $ do
        evaluate (LT.fromDSN "lol://haha") `shouldThrow` anyException
        evaluate (LT.fromDSN "http://haha") `shouldThrow` anyException

    it "parses example DSN" $ do
        LT.fromDSN dsn `shouldBe` ss

    it "generates correct endpoint" $ do
        LT.endpointURL LT.SentryDisabled `shouldBe` Nothing
        LT.endpointURL ss `shouldBe` Just "http://example.com/sentry/api/project-id/store/"
