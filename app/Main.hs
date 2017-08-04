{-# LANGUAGE OverloadedStrings #-}

-- | A simple reverse proxy to a server.

module Main where

import qualified Data.Text as T
import           Network.HTTP.Client
import           Network.Wai.Handler.Warp
import           Options.Applicative.Simple
import qualified Riker

-- | Main entry point.
main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  ((host', port', listenPort), ()) <-
    simpleOptions
      "0.0.0"
      "Riker"
      "Reverse proxy server"
      ((,,) <$> strOption (long "connect-host") <*>
       option auto (long "connect-port") <*>
       option auto (long "listen-port"))
      empty
  runEnv
    listenPort
    (Riker.rikerApplication
       manager
       Riker.Config
       { Riker.configHost = T.pack host'
       , Riker.configPort = port'
       , Riker.configUseSSL = False
       , Riker.configTimeout = Nothing
       })
