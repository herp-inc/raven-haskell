module System.Log.Lookout
    ( record
    ) where

import Data.UUID (UUID)
import System.Random (randomIO)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)

import System.Log.Lookout.Types

record :: String                         -- ^ Logger name.
       -> SentryLevel                    -- ^ Level
       -> String                         -- ^ Message
       -> (SentryRecord -> SentryRecord) -- ^ Additional options
       -> IO SentryRecord
record logger lvl msg upd = do
    eid <- show `fmap` (randomIO :: IO UUID)
    ts <- formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q" `fmap` getCurrentTime
    return $! upd (newRecord eid msg ts lvl logger)
