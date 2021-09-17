{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

-- | Internal representation of event record and related facilities.
--   Keep this under a pillow when writing custom wrappers.

module System.Log.Raven.Types
    ( SentrySettings(..), fromDSN, parseDSN, endpointURL
    , SentryService(..)
    , SentryLevel(..), SentryRecord(..), newRecord
    ) where

import Data.Aeson (ToJSON(toJSON), Value, object, (.=))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.String (fromString)
import Data.Time.Clock (UTCTime)

-- * Service settings

-- | Sentry client settings parsed from a DSN.
data SentrySettings = SentryDisabled
                    | SentrySettings { sentryURI        :: !String
                                     , sentryPublicKey  :: !String
                                     , sentryPrivateKey :: !String
                                     , sentryProjectId  :: !String
                                     } deriving (Show, Read, Eq)

-- | Transforms a service DSN into a settings. Format is:
--
-- > {PROTOCOL}://{PUBLIC_KEY}:{SECRET_KEY}@{HOST}{PATH}/{PROJECT_ID}
fromDSN :: String -> SentrySettings
fromDSN = either error id . parseDSN

parseDSN :: String -> Either String SentrySettings
parseDSN "" = return SentryDisabled
parseDSN dsn@(fst . break (== ':') -> proto)
    | proto == "http"  = makeSettings "http"  . splitAt 7 $ dsn
    | proto == "https" = makeSettings "https" . splitAt 8 $ dsn
    | otherwise = Left $ "parseDSN: unknown protocol (" ++ proto ++ ")"
    where
        body = break (== '@')

        keys = break (== ':') . fst . body
        pub  = fst . keys
        priv = drop 1 . snd . keys

        stuff = stuff' . break (=='/') . reverse . drop 1 . snd . body
        stuff' (a, b) = (reverse b, reverse a)
        uri = fst . stuff
        pid = snd . stuff

        assemble pref s = concat [pref, "://", uri s]

        makeSettings pref (_, s) = verify $! SentrySettings (assemble pref s) (pub s) (priv s) (pid s)

        verify (sentryURI -> "") = Left "Empty URI"
        verify (sentryPublicKey -> "") = Left "Empty public key"
        verify (sentryProjectId -> "") = Left "Empty project id"
        verify s = return s

-- | Assemble http endpoint URL from settings.
endpointURL :: SentrySettings -> Maybe String
endpointURL SentryDisabled = Nothing
endpointURL (SentrySettings uri _ _ pid) = Just $! concat [uri, "api/", pid, "/store/"]

-- * Logging service

-- | Misc settings packaged for easier operations.
data SentryService = SentryService { serviceSettings :: SentrySettings
                                   , serviceDefaults :: (SentryRecord -> SentryRecord)
                                   , serviceTransport :: (SentrySettings -> SentryRecord -> IO ())
                                   , serviceFallback :: (SentryRecord -> IO ())
                                   }

-- * Log entry

-- | Sentry log levels. Custom levels should be configured in Sentry or sending messages will fail.
data SentryLevel = Fatal
                 | Error
                 | Warning
                 | Info
                 | Debug
                 | Custom String
                 deriving (Show, Read, Eq)

instance ToJSON SentryLevel where
    toJSON Fatal = "fatal"
    toJSON Error = "error"
    toJSON Warning = "warning"
    toJSON Info = "info"
    toJSON Debug = "debug"
    toJSON (Custom s) = toJSON s

type Assoc = HM.HashMap String String

-- | Event packet to be sent. See detailed field descriptions in
--   <https://docs.sentry.io/clientdev/attributes/>
data SentryRecord = SentryRecord { srEventId     :: !String
                                 , srMessage     :: !String
                                 , srTimestamp   :: !UTCTime
                                 , srLevel       :: !SentryLevel
                                 , srLogger      :: !String
                                 , srPlatform    :: Maybe String
                                 , srCulprit     :: Maybe String
                                 , srTags        :: !Assoc
                                 , srServerName  :: Maybe String
                                 , srModules     :: !Assoc
                                 , srExtra       :: HM.HashMap String Value
                                 , srInterfaces  :: HM.HashMap String Value
                                 , srRelease     :: Maybe String
                                 , srEnvironment :: Maybe String
                                 } deriving (Show, Eq)

-- | Initialize a record with all required fields filled in.
newRecord :: String -> String -> UTCTime -> SentryLevel -> String -> SentryRecord
newRecord eid m t lev logger =
    SentryRecord
        eid m t lev logger
        Nothing Nothing HM.empty Nothing HM.empty HM.empty HM.empty Nothing Nothing

instance ToJSON SentryRecord where
    toJSON r = object . concat $
        [ [ "event_id" .= srEventId r
          , "message" .= srMessage r
          , "timestamp" .= srTimestamp r
          , "level" .= srLevel r
          , "logger" .= srLogger r
          ]
        , maybe [] (\v -> ["platform" .= v]) $ srPlatform r
        , maybe [] (\v -> ["culprit" .= v]) $ srCulprit r
        , if HM.null (srTags r) then [] else ["tags" .= srTags r]
        , maybe [] (\v -> ["server_name" .= v]) $ srServerName r
        , if HM.null (srModules r) then [] else ["modules" .= srModules r]
        , if HM.null (srExtra r) then [] else ["extra" .= srExtra r]
        , if HM.null (srInterfaces r) then
            []
          else
            [ fromString iface .= stuff
            | (iface, stuff) <- HM.toList $ srInterfaces r
            ]
        , maybe [] (\v -> ["release" .= v]) $ srRelease r
        , maybe [] (\v -> ["environment" .= v]) $ srEnvironment r
        ]
