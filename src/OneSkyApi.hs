{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ExtendedDefaultRules #-}


module OneSkyApi
    ( getFiles
    , ProjectId(..)
    , Credential(..)
    , getDevHash
    , Translations(..)
    )
where

import           Control.Category               ( (>>>) )
import           Control.Applicative            ( (<|>) )
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
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
                                                ( unpack
                                                , pack
                                                )
import           Data.Foldable                  ( asum )
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
                                                , httpJSON
                                                , Response
                                                )
import           Data.Digest.Pure.MD5
import           Control.Lens
import           Control.Monad                  ( mzero )
import           Data.Aeson.Lens                ( _String
                                                , key
                                                )
import qualified Data.ByteString               as B
import           Data.Map                       ( Map )
import           Data.Map                      as Map
import           Data.Aeson
import qualified Data.Aeson                    as AE
import qualified Data.Aeson.Types              as AET
import qualified Data.Traversable              as Traversable
import qualified Data.HashMap.Strict           as SHM
-- import qualified Data.HashMap.Lazy as HMap

-- default (Text.Text)

newtype ProjectId = ProjectId String deriving Show

apiBaseUrl = "https://platform.api.onesky.io/1"


data Credential = Credential { apiKey :: String, secret :: String }

newtype Translations = Translations (Map String String) deriving Show


translation1 :: Value -> AET.Parser String
translation1 = withObject
    "translation"
    (\translationObject -> fmap
        (B8.unpack . LBS.toStrict . encode)
        ((translationObject .: "translation") :: AET.Parser (Map String String))
    )

translation2 :: Value -> AET.Parser String
translation2 = withObject
    "translation"
    (\translationObject -> fmap
        (B8.unpack . LBS.toStrict . encode)
        ((translationObject .: "translation") :: AET.Parser (Map String [String]))
    )


translation :: Value -> AET.Parser String
translation v = (translation1 v)


instance FromJSON Translations where
    parseJSON (AE.Object obj) =
        Translations <$> Map.fromList <$> fmap (\(x, y) -> (Text.unpack x, y)) <$> SHM.toList <$> Traversable.mapM translation obj
    parseJSON _ = mzero

getFiles :: Credential -> ProjectId -> String -> IO Translations
getFiles (Credential apiKey secret) (ProjectId projectId) fileName = do
    (devHash, timestamp) <- fmap (getDevHash secret) getCurrentTimestamp
    resposne             <-
        httpJSON (getRequest devHash timestamp) :: IO (Response Translations)
    -- LBS.putStr $ getResponseBody resposne
    return $ (getResponseBody resposne)
  where
    requestUrl =
        -- apiBaseUrl <> "/projects/" <> projectId <> "/translations/multilingual"
        "http://localhost:3000/translations"
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
