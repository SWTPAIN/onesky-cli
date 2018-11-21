{-# LANGUAGE OverloadedStrings #-}

module OneSkyApi
    ( getFiles
    , Credential(..)
    )
where

import           Control.Category               ( (>>>) )
import qualified Data.ByteString.Base64        as Base64
                                                ( encode )
import qualified Data.ByteString.Char8         as B8
                                                ( pack
                                                , unpack
                                                )
import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import           Data.Text                     as Text
                                                ( unpack )
import qualified Data.ByteString.Lazy.Char8    as BL8
                                                ( pack
                                                , unpack
                                                , toStrict
                                                , fromStrict
                                                )
import           Data.Digest.Pure.MD5
import           Control.Lens
import           Data.Aeson.Lens                ( _String
                                                , key
                                                )
import qualified Network.Wreq                  as Wreq

apiBaseUrl = "https://platform.api.onesky.io/1"


data Credential = Credential { apiKey :: String, secret :: String }

getFiles :: Credential -> String -> IO String
getFiles credential fileName = do
    r <- Wreq.getWith opts "http://httpbin.org/get"
    return $ Text.unpack $ r ^. url
  where
    opts = Wreq.defaults & Wreq.param "foo" .~ ["bar", "quux"]
    url  = Wreq.responseBody . key "url" . _String

-- "https://platform.api.onesky.io/1/projects/142139/translations/multilingual?api_key=BD34rjKY6Jmbm3OQcmvuWdVorWLdxu3Z&source_file_name=bookmarks.json&file_format=I18NEXT_MULTILINGUAL_JSON"
getDevHash :: String -> Int -> (String, Int)
getDevHash secret timestamp =
    (show $ md5Digest $ secret <> (show timestamp), timestamp)

md5Digest :: String -> String
md5Digest = BL8.pack >>> md5 >>> md5DigestBytes >>> Base64.encode >>> B8.unpack

getCurrentTimestamp :: IO Integer
getCurrentTimestamp = round `fmap` getPOSIXTime
-- function _getDevHash (secret) {
--   var timestamp = Math.floor(Date.now() / 1000);
--   return {
--     devHash: md5(timestamp + secret),
--     timestamp: timestamp
--   };
-- }
