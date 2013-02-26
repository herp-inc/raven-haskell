{-# LANGUAGE OverloadedStrings #-}

-- | Structured data interfaces. Core Sentry interfaces:
--   <http://sentry.readthedocs.org/en/latest/developer/interfaces/index.html#provided-interfaces>

module System.Log.Lookout.Interfaces
    ( exception
    ) where

import Data.Aeson (object, (.=))
import qualified Data.HashMap.Strict as HM

import System.Log.Lookout.Types

-- | Register exception type, value and module.
exception :: String -> String -> String -> SentryRecord -> SentryRecord
exception t v m rec =
    rec { srInterfaces = HM.insert "sentry.interfaces.Exception" info $ srInterfaces rec }
    where
        info = object [ "type" .= t
                      , "value" .= v
                      , "module" .= m
                      ]
