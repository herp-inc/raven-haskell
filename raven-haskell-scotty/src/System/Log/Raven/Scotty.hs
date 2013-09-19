{-# LANGUAGE ScopedTypeVariables #-}

-- | Utilities to log errors in Scotty actions using raven-haskell.
-- 
-- > import Web.Scotty
-- >
-- > import System.Log.Raven
-- > import System.Log.Raven.Transport.HttpConduit (sendRecord)
-- > import System.Log.Raven.Scotty
-- >
-- > main = do
-- >     raven <- initRaven "https://…" id sendRecord stderrFallback
-- >     let hereBeDragons = guardIO raven "my.logger" (Just "DragonsError") (Just "My.Inner.Dragons")
-- >
-- >     scotty 8000 $ do
-- >         post "/some/action/" $ do
-- >             arg1 <- param "arg1"
-- >             arg2 <- param "arg2"
-- >             ds <- hereBeDragons $ dragonsIO arg1 arg2
-- >             if null ds
-- >                 then text "no dragons"
-- >                 else do
-- >                     let msg = "dragons! run!"
-- >                     scottyHttpInterface >>= logError raven "Main.main" msg

module System.Log.Raven.Scotty
    ( guardIO
    , logError
    , scottyHttpInterface
    ) where

import Web.Scotty (ActionM, request, reqHeader, params)

import System.Log.Raven as Raven
import System.Log.Raven.Types as Raven
import System.Log.Raven.Transport.HttpConduit (sendRecord)
import qualified System.Log.Raven.Interfaces as SI
import Network.Wai(Request(..))

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Lazy as TL
import qualified Data.CaseInsensitive as CI

import Control.Monad.Trans (liftIO)
import Control.Exception (try, throw, SomeException)

-- | A liftIO alternative that logs unhandled exceptions.
--   The function itself is verbose in arguments and designed to be curried and reused.
guardIO :: SentryService -- ^ Configured Sentry service.
        -> String        -- ^ Logger name.
        -> Maybe String  -- ^ Exception type name.
        -> Maybe String  -- ^ Action module name.
        -> IO a          -- ^ Action to run.
        -> ActionM a     -- ^ Result in a Scotty ActionM monad.
guardIO raven logger typename modname io = do
    res <- liftIO $ try io
    case res of
        Right r -> return r
        Left (e :: SomeException) -> do
            let exc = SI.exception (show e) typename modname
            logError raven logger (show e) exc
            liftIO $ throw e

-- | Log an error in an ActionM monad, collecting request data.
logError :: SentryService                  -- ^ A configured Sentry service.
         -> String                         -- ^ Logger name.
         -> String                         -- ^ Message to log.
         -> (SentryRecord -> SentryRecord) -- ^ Additional interfaces or other updates.
         -> ActionM ()
logError raven logger msg upd = do
    ifHttp <- scottyHttpInterface
    liftIO $ register raven logger Error msg (upd . ifHttp)

-- | Collect request parameters for a HTTP sentry interface.
scottyHttpInterface :: ActionM (SentryRecord -> SentryRecord)
scottyHttpInterface = do
    r <- request
    let method = BS.unpack $ requestMethod r
    let qs = case BS.unpack $ rawQueryString r of
                "" -> Nothing
                c -> Just c
    let hs = [ (BS.unpack . CI.original $ h, BS.unpack v)
             | (h, v) <- requestHeaders r
             ]

#if MIN_VERSION_scotty(0,5,0)
    host <- maybe (TL.pack "") id `fmap` reqHeader (TL.pack "Host")
#else
    host <- reqHeader (TL.pack "Host")
#endif

    let url = "http://" ++ TL.unpack host ++ BS.unpack (rawPathInfo r)

    ps <- params
    let args = SI.QueryArgs [ (TL.unpack k, TL.unpack v)
                            | (k, v) <- ps
                            ]

    return $ SI.http url method args qs Nothing hs []
