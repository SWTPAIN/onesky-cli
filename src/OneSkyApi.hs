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
import qualified Data.ByteString.Char8         as B8
                                                ( pack
                                                , unpack
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
import           Data.Digest.Pure.MD5
import           Control.Lens
import           Data.Aeson.Lens                ( _String
                                                , key
                                                )
import qualified Network.Wreq                  as Wreq

-- default (Text.Text)

newtype ProjectId = ProjectId String deriving Show

apiBaseUrl = "https://platform.api.onesky.io/1"


data Credential = Credential { apiKey :: String, secret :: String }

getFiles :: Credential -> ProjectId -> String -> IO String
getFiles (Credential apiKey secret) (ProjectId projectId) fileName = do
    putStrLn requestUrl
    (devHash, timestamp) <- fmap (getDevHash secret) getCurrentTimestamp
    r                    <- Wreq.getWith (getOpts devHash timestamp) requestUrl
    return $ BL8.unpack $ r ^. Wreq.responseBody
  where
    requestUrl =
        apiBaseUrl <> "/projects/" <> projectId <> "/translations/multilingual"
    getOpts = \devHash timestamp ->
        Wreq.defaults
            &  Wreq.params
            .~ [ ("api_key"         , Text.pack apiKey)
               , ("source_file_name", Text.pack fileName)
               , ("dev_hash"        , Text.pack devHash)
               , ("timestamp"       , Text.pack $ show timestamp)
               , ("file_format"     , "I18NEXT_MULTILINGUAL_JSON")
               ]

getDevHash :: String -> Integer -> (String, Integer)
getDevHash secret timestamp =
    (md5Digest $ (show timestamp) <> secret, timestamp)

md5Digest :: String -> String
md5Digest = BL8.pack >>> md5 >>> show

getCurrentTimestamp :: IO Integer
getCurrentTimestamp = round `fmap` getPOSIXTime
