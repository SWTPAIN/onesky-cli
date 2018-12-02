{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ExtendedDefaultRules #-}


module OneSkyApi
    ( getFiles
    , ProjectId(..)
    , Credential(..)
    , getDevHash
    )
where

import           Control.Category               ( (>>>) )
import qualified Data.ByteString.Base64        as Base64
                                                ( encode )
import qualified Data.ByteString               as S
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Char8         as B8
                                                ( pack
                                                , unpack
                                                , putStrLn
                                                )
import qualified Data.Text.Lazy.Encoding       as TLE
import           Data.Time.Clock.POSIX          ( getPOSIXTime )
import qualified Data.Text                     as Text
                                                ( unpack
                                                , pack
                                                )

import qualified Data.ByteString.Lazy.Char8    as BL8
                                                ( pack
                                                , unpack
                                                , toStrict
                                                , fromStrict
                                                )
import           Network.HTTP.Simple            ( httpLBS
                                                , setRequestQueryString
                                                , parseRequest_
                                                , getResponseBody
                                                , setRequestHeaders
                                                )
import           Data.Digest.Pure.MD5
import           Control.Lens
import           Data.Aeson.Lens                ( _String
                                                , key
                                                )
import qualified Data.ByteString               as B

-- default (Text.Text)

newtype ProjectId = ProjectId String deriving Show

apiBaseUrl = "https://platform.api.onesky.io/1"


data Credential = Credential { apiKey :: String, secret :: String }

getFiles :: Credential -> ProjectId -> String -> IO LBS.ByteString
getFiles (Credential apiKey secret) (ProjectId projectId) fileName = do
    (devHash, timestamp) <- fmap (getDevHash secret) getCurrentTimestamp
    putStrLn requestUrl
    putStrLn devHash
    print timestamp
    resposne <- httpLBS (getRequest devHash timestamp)
    -- LBS.putStr $ getResponseBody resposne
    return $ getResponseBody resposne
  where
    requestUrl =
        apiBaseUrl <> "/projects/" <> projectId <> "/translations/multilingual"
    getRequest devHash timestamp =
        (setRequestQueryString
                ([ ("api_key"         , Just $ B8.pack apiKey)
                 , ("source_file_name", Just $ B8.pack fileName)
                 , ("dev_hash"        , Just $ B8.pack devHash)
                 , ("timestamp"       , (Just $ B8.pack $ show timestamp))
                 , ("file_format", Just $ B8.pack "I18NEXT_MULTILINGUAL_JSON")
                 ]
                )
            )
            . (setRequestHeaders [("Content-Type", "application/json")])
            $ parseRequest_ requestUrl

getDevHash :: String -> Integer -> (String, Integer)
getDevHash secret timestamp =
    (md5Digest $ (show timestamp) <> secret, timestamp)

md5Digest :: String -> String
md5Digest = BL8.pack >>> md5 >>> show

getCurrentTimestamp :: IO Integer
getCurrentTimestamp = round `fmap` getPOSIXTime
