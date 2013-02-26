-- | Dummy «transports» for debugging purposes.

module System.Log.Lookout.Transport.Debug
    ( dumpRecord, briefRecord, catchRecord
    ) where

import Control.Concurrent.MVar (MVar, putMVar)

import System.Log.Lookout.Types

-- | Dump all glory details.
dumpRecord :: SentrySettings -> SentryRecord -> IO ()
dumpRecord _ rec = print rec

-- | Log-like output with very few data shown.
briefRecord :: SentrySettings -> SentryRecord -> IO ()
briefRecord _ rec = putStrLn $ concat [ srTimestamp rec, " "
                                      , show $ srLevel rec, " "
                                      , srLogger rec, ": "
                                      , srMessage rec
                                      ]

-- | Catch event record into an *empty* 'MVar'.
--   Make sure you take it's contents before next message!
catchRecord :: MVar SentryRecord -> SentrySettings -> SentryRecord -> IO ()
catchRecord var _ rec = putMVar var rec
