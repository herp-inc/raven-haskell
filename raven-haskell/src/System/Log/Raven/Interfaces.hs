{-# LANGUAGE OverloadedStrings #-}

-- | Structured data interfaces from Sentry core:
--   <http://sentry.readthedocs.org/en/latest/developer/interfaces/index.html#provided-interfaces>

module System.Log.Raven.Interfaces
    ( -- * Core sentry interfaces
      -- ** Message
      message
      -- ** Exception
    , exception
      -- ** Http
    , http, HttpArgs(..)
      -- ** User
    , user
      -- ** Query
    , query
      -- * Generic interface helpers
    , interface
    , fields, (.=:), fromMaybe, fromAssoc
    ) where

import Data.Aeson (ToJSON(toJSON), Value, object, (.=))
import qualified Data.HashMap.Strict as HM

import System.Log.Raven.Types

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
        info = fields [ "value" .=: v
                      , fromMaybe "type" t
                      , fromMaybe "module" m
                      ]

-- | Optional and optionally parsed HTTP query
data HttpArgs = EmptyArgs
              | RawArgs String
              | QueryArgs [(String, String)]
              deriving (Eq, Show)

-- | 'sentry.interfaces.Http':
--   The Request information is stored in the Http interface.
--
--   Sentry will explicitly look for REMOTE_ADDR in env for things
--   which require an IP address.
--
--   The data variable should only contain the request body
--   (not the query string). It can either be a dictionary
--   (for standard HTTP requests) or a raw request body.
--
-- > import System.Log.RavenInterfaces as SI
-- > let upd = SI.http
-- >             "http://absolute.uri/foo"
-- >             "POST"
-- >             (SI.QueryArgs [("foo", "bar")])
-- >             (Just "hello=world")
-- >             (Just "foo=bar")
-- >             [("Content-Type", "text/html")]
-- >             [("REMOTE_ADDR", "127.1.0.1")]
http :: String             -- ^ URL
     -> String             -- ^ Method
     -> HttpArgs           -- ^ Arguments
     -> Maybe String       -- ^ Query string
     -> Maybe String       -- ^ Cookies
     -> [(String, String)] -- ^ Headers
     -> [(String, String)] -- ^ Environment
     -> SentryRecord       -- ^ Record to update
     -> SentryRecord
http url m args q c hs env = interface "sentry.interfaces.Http" info
    where
        info = fields [ "url" .=: url
                      , "method" .=: m
                      , fromHttpArgs args
                      , fromMaybe "query_string" q
                      , fromMaybe "cookies" c
                      , fromAssoc "headers" hs
                      , fromAssoc "env" env
                      ]

        fromHttpArgs EmptyArgs = []
        fromHttpArgs (RawArgs s) = "data" .=: s
        fromHttpArgs (QueryArgs kvs) = "data" .=: HM.fromList kvs

-- | 'sentry.interfaces.User':
--   An interface which describes the authenticated User for a request.
--
-- > let upd = SI.user "unique_id" [ ("username", "my_user")
-- >                               , ("email", "foo@example.com") ]
user :: String             -- ^ User's unique identifier
     -> [(String, String)] -- ^ Optional user data
     -> SentryRecord       -- ^ Record to update
     -> SentryRecord
user uid kwargs = interface "sentry.interfaces.User" info
    where
        info = HM.fromList $ ("id", uid) : kwargs

-- | 'sentry.interfaces.Query':
--   A SQL query with an optional string describing the SQL driver, engine.
query :: Maybe String -- ^ SQL Driver
      -> String       -- ^ Query
      -> SentryRecord -- ^ Record to update
      -> SentryRecord
query d q = interface "sentry.interfaces.Query" info
    where
        info = fields [ "query" .=: q
                      , fromMaybe "engine" d
                      ]

-- | Generic interface helper.
interface :: (ToJSON v) => String -> v -> SentryRecord -> SentryRecord
interface k v rec =
    rec { srInterfaces = HM.insert k (toJSON v) $ srInterfaces rec }

-- | JSON object with optional fields removed.
fields :: [[(String, Value)]] -> HM.HashMap String Value
fields = HM.fromList . concat

-- | A mandatory field.
(.=:) :: (ToJSON v) => String -> v -> [(String, Value)]
k .=: v = [(k, toJSON v)]

-- | Optional simple field.
fromMaybe :: (ToJSON v) => String -> Maybe v -> [(String, Value)]
fromMaybe k = maybe [] (\v -> [ (k, toJSON v) ])

-- | Optional dict-like field.
fromAssoc :: String -> [(String, String)] -> [(String, Value)]
fromAssoc _ [] = []
fromAssoc k vs = [(k, toJSON . HM.fromList $ vs)]
