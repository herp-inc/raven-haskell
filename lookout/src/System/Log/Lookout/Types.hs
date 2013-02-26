{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module System.Log.Lookout.Types
    ( SentrySettings(..), fromDSN, endpointURL
    , SentryService(..)
    , SentryLevel(..), SentryRecord(..), newRecord
    ) where

import Data.Aeson (ToJSON(toJSON), object, (.=))
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

-- * Service settings

data SentrySettings = SentryDisabled
                    | SentrySettings { sentryURI        :: !String
                                     , sentryPublicKey  :: !String
                                     , sentryPrivateKey :: !String
                                     , sentryProjectId  :: !String
                                     } deriving (Show, Read, Eq)

-- | Transforms a service DSN into a settings. Format is:
--   '{PROTOCOL}://{PUBLIC_KEY}:{SECRET_KEY}@{HOST}/{PATH}{PROJECT_ID}'
fromDSN :: String -> SentrySettings
fromDSN "" = SentryDisabled
fromDSN dsn@(fst . break (== ':') -> proto)
    | proto == "http"  = makeSettings "http"  . splitAt 7 $ dsn
    | proto == "https" = makeSettings "https" . splitAt 8 $ dsn
    | otherwise = error $ "fromDSN: unknown protocol (" ++ proto ++ ")"
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

        verify (sentryURI -> "") = error "Empty URI"
        verify (sentryPublicKey -> "") = error "Empty public key"
        verify (sentryPrivateKey -> "") = error "Empty private key"
        verify (sentryProjectId -> "") = error "Empty project id"
        verify s = s

endpointURL :: SentrySettings -> Maybe String
endpointURL SentryDisabled = Nothing
endpointURL (SentrySettings uri _ _ pid) = Just $! concat [uri, "api/", pid, "/store/"]

-- * Logging service

data SentryService = SentryService { serviceSettings :: SentrySettings
                                   , serviceDefaults :: (SentryRecord -> SentryRecord)
                                   , serviceTransport :: (SentrySettings -> SentryRecord -> IO ())
                                   , serviceFallback :: (SentryRecord -> IO ())
                                   }

-- * Log entry

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

data SentryRecord = SentryRecord { srEventId    :: !String
                                 , srMessage    :: !String
                                 , srTimestamp  :: !String
                                 , srLevel      :: !SentryLevel
                                 , srLogger     :: !String
                                 , srPlatform   :: Maybe String
                                 , srCulprit    :: Maybe String
                                 , srTags       :: !Assoc
                                 , srServerName :: Maybe String
                                 , srModules    :: !Assoc
                                 , srExtra      :: !Assoc
                                 , srInterfaces :: HM.HashMap String Assoc
                                 } deriving (Show, Eq)

newRecord :: String -> String -> String -> SentryLevel -> String -> SentryRecord
newRecord eid m t lev logger =
    SentryRecord
        eid m t lev logger
        Nothing Nothing HM.empty Nothing HM.empty HM.empty HM.empty

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
        , if HM.null (srInterfaces r) then [] else [ T.pack iface .= stuff
                                                   | (iface, stuff) <- HM.toList $ srInterfaces r]
        ]
