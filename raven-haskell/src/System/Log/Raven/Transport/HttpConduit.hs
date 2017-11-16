-- | HTTPS-capable transport using http-conduit.

{-# LANGUAGE OverloadedStrings #-}
module System.Log.Raven.Transport.HttpConduit
    ( sendRecord
    , sendRecordWith
    ) where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as BS

import System.Log.Raven
import System.Log.Raven.Types

sendRecord :: SentrySettings -> SentryRecord -> IO ()
sendRecord conf rec = do
    manager <- newManager tlsManagerSettings
    runResourceT $ sendRecordWith manager conf rec

sendRecordWith :: MonadIO m => Manager -> SentrySettings -> SentryRecord -> m ()
sendRecordWith manager conf rec = do
    let ep = endpointURL conf
    let auth = concat [ "Sentry sentry_version=2.0"
                      , ", sentry_client=raven-haskell-0.1.0.0"
                      , ", sentry_key=" ++ sentryPublicKey conf
                      , ", sentry_secret=" ++ sentryPrivateKey conf
                      ]
    case ep of
        Nothing -> return ()
        Just url -> do
            req' <- liftIO $ parseUrlThrow url
            let req = req' { method = "POST"
                           , requestHeaders = [("X-Sentry-Auth", BS.pack auth)]
                           , requestBody = RequestBodyLBS (recordLBS rec)
                           }
            _ <- httpLbs req manager
            return ()
