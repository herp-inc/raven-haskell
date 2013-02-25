import Test.Hspec
import Control.Exception (evaluate)

import System.Log.Lookout (record)
import System.Log.Lookout.Types as LT

main :: IO ()
main = hspec $ do
  describe "Types" $ do
    it "parses empty DSN into SentryDisabled" $ do
        fromDSN "" `shouldBe` SentryDisabled

    it "carps on invalid DSN" $ do
        evaluate (fromDSN "lol://haha") `shouldThrow` anyException
        evaluate (fromDSN "http://haha") `shouldThrow` anyException

    it "parses example DSN" $ do
        fromDSN dsn `shouldBe` ss

    it "generates correct endpoint" $ do
        endpointURL SentryDisabled `shouldBe` Nothing
        endpointURL ss `shouldBe` Just "http://example.com/sentry/api/project-id/store/"

    it "generates empty record" $ do
        newRecord "" "" "" Debug "" `shouldBe` emptyRecord

    it "generates record template" $ do
        r <- record "test.logger" Debug "test record please ignore" $ \rec ->
            rec { srEventId = ""
                , srTimestamp = ""
                }
        r `shouldBe` emptyRecord { srMessage = "test record please ignore"
                                 , srLevel = Debug
                                 , srLogger = "test.logger"
                                 }

dsn = "http://public_key:secret_key@example.com/sentry/project-id"

ss = SentrySettings {
         sentryURI = "http://example.com/sentry/",
         sentryPublicKey = "public_key",
         sentryPrivateKey = "secret_key",
         sentryProjectId = "project-id"
     }

emptyRecord = SentryRecord { srEventId    = ""
                              , srMessage    = ""
                              , srTimestamp  = ""
                              , srLevel      = Debug
                              , srLogger     = ""
                              , srPlatform   = Nothing
                              , srCulprit    = Nothing
                              , srTags       = []
                              , srServerName = Nothing
                              , srModules    = []
                              , srExtra      = []
                              , srInterfaces = []
                              }
