{-# LANGUAGE OverloadedStrings #-}

module Riker
  ( Config(..)
  , rikerApplication
  ) where

import qualified Control.Exception as Exception
import           Control.Monad (unless)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy.Builder as Builder
import           Data.Function (fix)
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client
import qualified Network.HTTP.Client.Conduit as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai

-- | Configuration for riker.
data Config = Config
  { configHost :: Text
  , configPort :: Int
  , configUseSSL :: Bool
  , configTimeout :: Maybe Int
  } deriving (Eq, Ord, Show)

-- | A WAI middleware for using riker with warp.
rikerApplication :: HTTP.Manager -> Config -> Wai.Application
rikerApplication mgr rpConfig request sendResponse =
  Exception.bracket
    (Network.HTTP.Client.responseOpen proxiedRequest mgr)
    HTTP.responseClose $ \res ->
    sendResponse $
    Wai.responseStream
      (HTTP.responseStatus res)
      (rewriteHeaders $ HTTP.responseHeaders res)
      (sendBody $ HTTP.responseBody res)
  where
    proxiedRequest = proxyRequest rpConfig request
    sendBody body send _flush =
      fix $ \loop -> do
        bs <- body
        unless (ByteString.null bs) $ do
          () <- send $ Builder.byteString bs
          loop

-- | Proxy the WAI request to a HTTP client request to the configured server.
proxyRequest :: Config -> Wai.Request -> HTTP.Request
proxyRequest rpConfig request =
  HTTP.defaultRequest
  { HTTP.checkResponse = \_ _ -> return ()
  , HTTP.responseTimeout =
      maybe HTTP.responseTimeoutNone HTTP.responseTimeoutMicro $
      configTimeout rpConfig
  , HTTP.method = Wai.requestMethod request
  , HTTP.secure = configUseSSL rpConfig
  , HTTP.host = T.encodeUtf8 $ configHost rpConfig
  , HTTP.port = configPort rpConfig
  , HTTP.path = Wai.rawPathInfo request
  , HTTP.queryString = Wai.rawQueryString request
  , HTTP.requestHeaders =
      filterHeaders $ rewriteHeaders (Wai.requestHeaders request)
  , HTTP.requestBody =
      case Wai.requestBodyLength request of
        Wai.ChunkedBody ->
          HTTP.RequestBodyStreamChunked ($ Wai.requestBody request)
        Wai.KnownLength n ->
          HTTP.RequestBodyStream (fromIntegral n) ($ Wai.requestBody request)
  , HTTP.decompress = const False
  , HTTP.redirectCount = 0
  , HTTP.cookieJar = Nothing
  }

-- | TODO: Implement a rewrite function here.
rewriteHeaders :: [HTTP.Header] -> [HTTP.Header]
rewriteHeaders = id

-- | Filter headers that shouldn't be proxied to the internal server.
filterHeaders :: [HTTP.Header] -> [HTTP.Header]
filterHeaders = filter useHeader
  where
    useHeader ("Transfer-Encoding", _) = False
    useHeader ("Content-Length", _) = False
    useHeader ("Host", _) = False
    useHeader _ = True
