-- | Raven is a client for Sentry event server (<https://www.getsentry.com/>).
--
--   Start by initializing the raven 'Service':
--
-- > l <- initRaven
-- >          "https://pub:priv@sentry.hostname.tld:8443/sentry/example_project"
-- >          id
-- >          sendRecord
-- >          stderrFallback
--
--   Send events using 'register' function:
--
-- > register l "my.logger.name" Debug "Hi there!" id
--
--   Tags and stuff can be added using register update functions.
--
-- > import Data.HashMap.Strict as HM
-- > let tags r = r { srTags = HM.insert "spam" "sausage"
-- >                         . HM.insert "eggs" "bacon"
-- >                         . srTags r }
-- > lt <- initRaven dsn tags sendRecord stderrFallback
-- >
-- > let culprit r = r { srCulprit = "my.module.function.name" }
-- > register lt "test.culprit" Error "It's a trap!" culprit
-- > let extra r = r { srExtra = HM.insert "fnord" "42" $ srExtra r }
-- > register lt "test.extra" Info "Test with tags and extra, please ignore."
--
--   The core package provides only general interface for sending events which
--   could be wrapped to adapt it to your needs.
--
-- > let debug msg = forkIO $ register l "my.logger.name" Debug msg (culprit . extra)
-- > debug "Async stuff too."
--
--   There are some little helpers to compose your own updaters.
--   You can use them both in 'initRaven' and 'register'.
--
-- > l <- initRaven dsn ( tags [ ("spam", "sausage"
-- >                           , ("eggs", "bacon") ]
-- >                    . extra [ ("more", "stuff") ]
-- >                    )
-- >                    sendRecord stderrFallback
-- >
-- > register l "test.helpers" Info "yup, i'm here." $ culprit "java.lang.NotReally"

module System.Log.Raven
    ( -- * Event service
      initRaven, disabledRaven
    , register
      -- * Fallback handlers
    , stderrFallback, errorFallback, silentFallback
      -- * Record updaters
    , culprit, tags, extra
      -- * Lower level helpers
    , record, recordLBS
    ) where

import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)

import Data.UUID.Types (UUID)
import System.Random (randomIO)
import Data.Time.Clock (getCurrentTime)
import System.IO (stderr, hPutStrLn)
import qualified Control.Exception as E
import qualified Data.HashMap.Strict as HM

import System.Log.Raven.Types

-- | Initialize event service.
initRaven :: String                                    -- ^ Sentry DSN
          -> (SentryRecord -> SentryRecord)            -- ^ Default fields updater. Use 'id' if not needed.
          -> (SentrySettings -> SentryRecord -> IO ()) -- ^ Event transport from Raven.Transport.*
          -> (SentryRecord -> IO ())                   -- ^ Fallback handler.
          -> IO SentryService                          -- ^ Event service to use in 'register'.
initRaven dsn d t fb = return
    SentryService { serviceSettings = fromDSN dsn
                  , serviceDefaults = d
                  , serviceTransport = t
                  , serviceFallback = fb
                  }

-- | Disabled service that ignores incoming events.
disabledRaven :: IO SentryService
disabledRaven = initRaven "" id undefined undefined

-- | Ask service to store an event.
register :: SentryService                  -- ^ Configured raven service.
         -> String                         -- ^ Logger name.
         -> SentryLevel                    -- ^ Sentry event level.
         -> String                         -- ^ Message.
         -> (SentryRecord -> SentryRecord) -- ^ Record updates.
         -> IO ()
register s loggerName level message upd = do
    rec <- record loggerName level message (upd . serviceDefaults s)

    let transport = serviceTransport s

    case serviceSettings s of
        SentryDisabled -> return ()
        settings -> E.catch (transport settings rec)
                            (\(E.SomeException _) -> serviceFallback s $ rec)

-- | Show basic message on stderr.
stderrFallback :: SentryRecord -> IO ()
stderrFallback rec =
    hPutStrLn stderr $ concat
        [ show $ srTimestamp rec, " "
        , show $ srLevel rec, " "
        , srLogger rec, ": "
        , srMessage rec
        ]

-- | Crash and burn with record data.
errorFallback :: SentryRecord -> IO ()
errorFallback rec = error $ "Error sending record: " ++ show rec

-- | Ignore recording errors.
silentFallback :: SentryRecord -> IO ()
silentFallback _ = return ()

-- | Record an event using logging service.
record :: String                         -- ^ Logger name.
       -> SentryLevel                    -- ^ Level
       -> String                         -- ^ Message
       -> (SentryRecord -> SentryRecord) -- ^ Additional options
       -> IO SentryRecord
record logger lvl msg upd = do
    eid <- (filter (/= '-') . show) `fmap` (randomIO :: IO UUID)
    ts <- getCurrentTime
    return $! upd (newRecord eid msg ts lvl logger)

-- | JSON-encode record data.
recordLBS :: SentryRecord -> ByteString
recordLBS = encode

-- | Set culprit field.
culprit :: String -> SentryRecord -> SentryRecord
culprit c r = r { srCulprit = Just c }

-- | Add record tags.
tags :: [(String, String)] -> SentryRecord -> SentryRecord
tags ts r = r { srTags = HM.fromList ts `HM.union` srTags r }

-- | Add record extra information.
extra :: [(String, String)] -> SentryRecord -> SentryRecord
extra ts r =  r { srExtra = HM.fromList ts `HM.union` srExtra r }
