{-# LANGUAGE OverloadedStrings #-}

-- | Structured data interfaces. Core Sentry interfaces:
--   <http://sentry.readthedocs.org/en/latest/developer/interfaces/index.html#provided-interfaces>

module System.Log.Lookout.Interfaces
    ( interface
    , message, messageS
    , exception
    ) where

import Data.Aeson (ToJSON(toJSON), Value, object, (.=))
import qualified Data.HashMap.Strict as HM

import System.Log.Lookout.Types

-- | Generic interface helper.
interface :: (ToJSON v) => String -> v -> SentryRecord -> SentryRecord
interface k v rec =
    rec { srInterfaces = HM.insert k (toJSON v) $ srInterfaces rec }

-- | 'sentry.interfaces.Message':
--   A standard message consisting of a message arg, and an optional params
--   arg for formatting.
message :: String       -- ^ Message text (no more than 1000 characters in length).
        -> [Value]      -- ^ Formatting arguments
        -> SentryRecord -- ^ Record to update
        -> SentryRecord
message msg args = interface "sentry.interfaces.Message" info
    where
        info = object [ "message" .= take 1000 msg
                      , "params" .= args
                      ]

-- | 'sentry.interfaces.Message'-variation with all-strings arguments.
messageS :: String       -- ^ Message text (no more than 1000 characters in length).
         -> [String]     -- ^ Formatting arguments
         -> SentryRecord -- ^ Record to update
         -> SentryRecord
messageS msg args = message msg (map toJSON args)

-- | 'sentry.interfaces.Exception':
--   A standard exception with a mandatory value argument,
--   and optional type and``module`` argument describing
--   the exception class type and module namespace.
exception :: String       -- ^ Value
          -> Maybe String -- ^ Type
          -> Maybe String -- ^ Module
          -> SentryRecord -- ^ Record to update
          -> SentryRecord
exception v t m = interface "sentry.interfaces.Exception" info
    where
        info :: HM.HashMap String String
        info = HM.fromList . concat $ [ [ ("value", v) ]
                                      , maybe [] (\s -> [ ("type", s) ]) t
                                      , maybe [] (\s -> [ ("module", s) ]) m
                                      ]
