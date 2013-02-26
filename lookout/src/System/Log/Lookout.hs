-- | Lookout is a client for Sentry event server (<https://www.getsentry.com/>).
--
--   Start by initializing the lookout 'Service':
--
-- > l <- initLookout
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
-- > lt <- initLookout dsn tags sendRecord stderrFallback
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

module System.Log.Lookout
    ( -- * Event service
      initLookout, disabledLookout
    , register
      -- * Fallback handlers
    , stderrFallback, errorFallback, silentFallback
      -- * Lower level helpers
    , record, recordLBS
    ) where

import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)

import Data.UUID (UUID)
import System.Random (randomIO)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import System.IO (stderr, hPutStrLn)
import qualified Control.Exception as E

import System.Log.Lookout.Types

-- | Initialize event service.
initLookout :: String                                    -- ^ Sentry DSN
            -> (SentryRecord -> SentryRecord)            -- ^ Default fields updater. Use 'id' if not needed.
            -> (SentrySettings -> SentryRecord -> IO ()) -- ^ Event transport from Looklout.Transport.*
            -> (SentryRecord -> IO ())                   -- ^ Fallback handler.
            -> IO SentryService                          -- ^ Event service to use in 'register'.
initLookout dsn d t fb = return
    SentryService { serviceSettings = fromDSN dsn
                  , serviceDefaults = d
                  , serviceTransport = t
                  , serviceFallback = fb
                  }

-- | Disabled service that ignores incoming events.
disabledLookout :: IO SentryService
disabledLookout = initLookout "" id undefined undefined

-- | Ask service to store an event.
register :: SentryService                  -- ^ Configured lookout service.
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
        [ srTimestamp rec, " "
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
    ts <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" `fmap` getCurrentTime
    return $! upd (newRecord eid msg ts lvl logger)

-- | JSON-encode record data.
recordLBS :: SentryRecord -> ByteString
recordLBS = encode