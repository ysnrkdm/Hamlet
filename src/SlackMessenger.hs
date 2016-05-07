{-# LANGUAGE OverloadedStrings #-}

module SlackMessenger where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import System.IO.Unsafe
import Data.ByteString.Char8

sendMessage text = do
  initReq <- parseUrl "https://hooks.slack.com/services/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"

  let req' = initReq { secure = True } -- Turn on https
  let req = (flip urlEncodedBody) req' [ ("text", text) ]

  response <- withManager $ httpLbs req

  L.putStr $ responseBody response

unsafeSendMessageNow :: String -> a -> a
unsafeSendMessageNow text expr = unsafePerformIO $ do
    sendMessage $ pack text
    return expr
