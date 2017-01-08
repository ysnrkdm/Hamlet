{-# LANGUAGE OverloadedStrings #-}

module SlackMessenger where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L
import System.IO.Unsafe
import Data.Aeson

data Payload = Payload { test :: String } deriving Show
instance FromJSON Payload where
    parseJSON (Object v) = Payload <$> (v .: "text")

instance ToJSON Payload where
    toJSON (Payload text) = object [ "text" .= text ]

sendMessage :: String -> IO ()
sendMessage text = do
    initReq <- parseUrl "https://hooks.slack.com/services/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"
    let payload = Payload text

    let req' = initReq { secure = True, method = "POST" } -- Turn on https
    let req = (flip urlEncodedBody) req' [ ("payload", L.toStrict $ encode payload) ]

    response <- withManager $ httpLbs req

    L.putStr $ responseBody response

unsafeSendMessageNow :: String -> a -> a
unsafeSendMessageNow text expr = unsafePerformIO $ do
    sendMessage text
    return expr
