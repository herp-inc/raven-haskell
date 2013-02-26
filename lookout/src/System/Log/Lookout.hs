module System.Log.Lookout
    ( record, recordLBS
    , stderrFallback, errorFallback, silentFallback
    , initLookout, disabledLookout
    , register
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

record :: String                         -- ^ Logger name.
       -> SentryLevel                    -- ^ Level
       -> String                         -- ^ Message
       -> (SentryRecord -> SentryRecord) -- ^ Additional options
       -> IO SentryRecord
record logger lvl msg upd = do
    eid <- (filter (/= '-') . show) `fmap` (randomIO :: IO UUID)
    ts <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" `fmap` getCurrentTime
    return $! upd (newRecord eid msg ts lvl logger)

recordLBS :: SentryRecord -> ByteString
recordLBS = encode

stderrFallback :: SentryRecord -> IO ()
stderrFallback rec =
    hPutStrLn stderr $ concat
        [ srTimestamp rec, " "
        , show $ srLevel rec, " "
        , srLogger rec, ": "
        , srMessage rec
        ]

errorFallback :: SentryRecord -> IO ()
errorFallback rec = error $ "Error sending record: " ++ show rec

silentFallback :: SentryRecord -> IO ()
silentFallback _ = return ()

disabledLookout :: IO SentryService
disabledLookout = initLookout "" id undefined undefined

initLookout :: String
            -> (SentryRecord -> SentryRecord)
            -> (SentrySettings -> SentryRecord -> IO ())
            -> (SentryRecord -> IO ())
            -> IO SentryService
initLookout dsn d t fb = return
    SentryService { serviceSettings = fromDSN dsn
                  , serviceDefaults = d
                  , serviceTransport = t
                  , serviceFallback = fb
                  }

register :: SentryService
         -> String
         -> SentryLevel
         -> String
         -> (SentryRecord -> SentryRecord)
         -> IO ()
register s loggerName level message upd = do
    rec <- record loggerName level message (upd . serviceDefaults s)

    let transport = serviceTransport s

    case serviceSettings s of
        SentryDisabled -> return ()
        settings -> E.catch (transport settings rec)
                            (\(E.SomeException _) -> serviceFallback s $ rec)
