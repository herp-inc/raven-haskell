{-# LANGUAGE ViewPatterns #-}
module System.Log.Lookout.Types
    ( SentrySettings(..), fromDSN, endpointURL
    , SentryLevel(..), SentryRecord(..), newRecord, record
    ) where

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

-- * Log entry

data SentryLevel = Fatal
                 | Error
                 | Warning
                 | Info
                 | Debug
                 | Custom String
                 deriving (Show, Read, Eq)

type Assoc = [(String, String)]

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
                                 , srInterfaces :: [(String, Assoc)]
                                 } deriving (Show, Eq)

newRecord :: String -> String -> String -> SentryLevel -> String -> SentryRecord
newRecord eid m t lev logger =
    SentryRecord
        eid m t lev logger
        Nothing Nothing [] Nothing [] [] []

record :: String                         -- ^ Logger name.
       -> SentryLevel                    -- ^ Level
       -> String                         -- ^ Message
       -> (SentryRecord -> SentryRecord) -- ^ Additional options
       -> IO SentryRecord
record logger lvl msg upd = do
    eid <- return "0123456789abcdef0123456789abcdef"
    ts <- return "YYYY-mm-dd HH:MM:SS.ns"
    return $! upd (newRecord eid msg ts lvl logger)
