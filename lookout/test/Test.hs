{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Control.Exception (evaluate)

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.HashMap.Strict as HM
import Data.Aeson (object, (.=))
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)

import System.Log.Lookout
import qualified System.Log.Lookout.Interfaces as SI
import System.Log.Lookout.Types as LT
import System.Log.Lookout.Transport.Debug (dumpRecord, briefRecord, catchRecord)
import System.Log.Lookout.Transport.HttpConduit (sendRecord)

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
        r <- strip `fmap` test
        r `shouldBe` testRecord

    it "serializes as JSON" $ do
        r <- strip `fmap` test
        recordLBS r `shouldBe` testRecordLBS

  describe "Service" $ do
    it "ignores record when service is disabled" $ do
        l <- disabledLookout
        ok <- register l "test.logger" Debug "Fly me to /dev/null" id
        ok `shouldBe` ()

    it "uses a fallback" $ do
        v <- newEmptyMVar
        l <- initLookout "http://bad:boo@localhost:1/lookout" id undefined (\_ -> putMVar v True)
        sent <- register l "test.logger" Debug "Ready or not, here i come!" id
        sent `shouldBe` ()
        fbUsed <- takeMVar v
        fbUsed `shouldBe` True

  describe "Transport: HttpConduit" $ do
    it "sends a record" $ do
        l <- initLookout "http://test:test@localhost:19876/lookout" id sendRecord errorFallback
        ok <- register l "test.logger" Debug "Like a record, baby, right round." id
        ok `shouldBe` ()

  describe "Transport: Debug" $ do
    it "shows brief message" $ do
        l <- initLookout "http://not:really@whatever.fake/project" id briefRecord errorFallback
        ok <- register l "test.logger" Debug "Hi there!" id
        ok `shouldBe` ()

    it "dumps event record" $ do
        l <- initLookout "http://not:really@whatever.fake/project" id dumpRecord errorFallback
        ok <- register l "test.logger" Debug "Hi there!" id
        ok `shouldBe` ()

    it "catches record into MVar" $ do
        v <- newEmptyMVar
        l <- initLookout "http://not:really@whatever.fake/project" id (catchRecord v) errorFallback
        ok <- register l "test.logger" Debug "Hi there!" id
        ok `shouldBe` ()
        rec <- takeMVar v
        srMessage rec `shouldBe` "Hi there!"

  describe "Updaters" $ do
    let make = record "test.logger" Debug "test record please ignore"

    it "updates culprit" $ do
       r <- make $ culprit "Test.main"
       srCulprit r `shouldBe` Just "Test.main"

    it "updates tags" $ do
        r <- make $ tags []
        srTags r `shouldBe` HM.empty

        r <- make $ tags [("test", "shmest")]
        srTags r `shouldBe` HM.fromList [("test", "shmest")]

    it "updates extra" $ do
        r <- make $ extra []
        srExtra r `shouldBe` HM.empty

        r <- make $ extra [("bru", "haha")]
        srExtra r `shouldBe` HM.fromList [("bru", "haha")]

    it "fills in service defaults" $ do
        v <- newEmptyMVar
        l <- initLookout
                 "http://not:really@whatever.fake/project"
                 ( tags [("spam", "eggs"), ("test", "")]
                 . extra [("sausage", "salad")] )
                 (catchRecord v)
                 errorFallback
        r <- register l "test.logger" Debug "test record please ignore" $ tags [("test", "shmest")]
        rec <- takeMVar v
        srTags rec `shouldBe` HM.fromList [("spam", "eggs"), ("test", "shmest")]
        srExtra rec `shouldBe` HM.fromList [("sausage", "salad")]

  describe "Interfaces" $ do
    let make = record "test.logger" Debug "test record please ignore"
    let send r = do
        ok <- sendRecord (fromDSN "http://test:test@localhost:19876/lookout") r
        ok `shouldBe` ()

    it "registers messages" $ do
        r <- make $ SI.message "My raw message with interpreted strings like %s" ["this"]

        let ex = object [ "message" .= ("My raw message with interpreted strings like %s" :: String)
                        , "params" .= ["this" :: String]
                        ]
        HM.lookup "sentry.interfaces.Message" (srInterfaces r) `shouldBe` Just ex

        send r

    it "registers exceptions" $ do
        r <- make $ SI.exception "Wattttt!" (Just "SyntaxError") (Just "__buitins__")

        let ex = object [ "type" .= ("SyntaxError" :: String)
                        , "value" .= ("Wattttt!"  :: String)
                        , "module" .= ("__buitins__"  :: String)
                        ]

        HM.lookup "sentry.interfaces.Exception" (srInterfaces r) `shouldBe` Just ex

        send r

    it "registers HTTP queries" $ do
        r <- make $ SI.http
                      "http://example.com/fake/url"
                      "POST"
                      (SI.QueryArgs [("foo", "bar")])
                      (Just "hello=world")
                      (Just "foo=bar")
                      [("Content-Type", "text/html")]
                      [("REMOTE_ADDR", "127.1.0.1")]

        let ex = object [ "url"          .= ("http://example.com/fake/url" :: String)
                        , "method"       .= ("POST" :: String)
                        , "data"         .= object [ "foo" .= ("bar" :: String) ]
                        , "query_string" .= ("hello=world" :: String)
                        , "cookies"      .= ("foo=bar" :: String)
                        , "headers"      .= object [ "Content-Type" .= ("text/html" :: String) ]
                        , "env"          .= object [ "REMOTE_ADDR"  .= ("127.1.0.1" :: String)]
                        ]
        HM.lookup "sentry.interfaces.Http" (srInterfaces r) `shouldBe` Just ex

        send r

dsn = "http://public_key:secret_key@example.com/sentry/project-id"

ss = SentrySettings {
         sentryURI = "http://example.com/sentry/",
         sentryPublicKey = "public_key",
         sentryPrivateKey = "secret_key",
         sentryProjectId = "project-id"
     }

strip rec = rec { srEventId = ""
                , srTimestamp = ""
                }

test = record "test.logger" Debug "test record please ignore" id

emptyRecord = SentryRecord { srEventId    = ""
                              , srMessage    = ""
                              , srTimestamp  = ""
                              , srLevel      = Debug
                              , srLogger     = ""
                              , srPlatform   = Nothing
                              , srCulprit    = Nothing
                              , srTags       = HM.empty
                              , srServerName = Nothing
                              , srModules    = HM.empty
                              , srExtra      = HM.empty
                              , srInterfaces = HM.empty
                              }

testRecord = emptyRecord { srMessage = "test record please ignore"
                         , srLevel = Debug
                         , srLogger = "test.logger"
                         }

testRecordLBS = LBS.pack "{\"timestamp\":\"\",\"message\":\"test record please ignore\",\"event_id\":\"\",\"level\":\"debug\",\"logger\":\"test.logger\"}"
