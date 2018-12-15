{-# LANGUAGE OverloadedStrings #-}

module OneSkyApi
    ( getFiles
    , ProjectId(..)
    , Credential(..)
    , getDevHash
    , FileTranslation(..)
    , TranslationContent(..)
    )
where

import           Control.Category               ( (>>>) )
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
                                                ( pack )
import           Network.HTTP.Simple            ( setRequestQueryString
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
import           Data.Aeson.Encode.Pretty       ( encodePretty )
import qualified Data.Aeson                    as AE
import qualified Data.Aeson.Types              as AET
import qualified Data.Traversable              as Traversable
import qualified Data.HashMap.Strict           as SHM
import           Data.Text.Encoding             ( decodeUtf8 )
import           Control.Concurrent             ( forkIO )
import           Control.Concurrent.Async       ( mapConcurrently )

newtype ProjectId = ProjectId String deriving Show

apiBaseUrl = "https://platform.api.onesky.io/1"


data Credential = Credential { apiKey :: String, secret :: String }

data FileTranslation = FileTranslation {
    fileName :: String,
    translationContent :: TranslationContent
}

newtype TranslationContent = TranslationContent (Map String Text) deriving Show


translation :: Value -> AET.Parser Text
translation = withObject
    "translation"
    (\translationObject -> fmap
        (decodeUtf8 . LBS.toStrict . encodePretty)
        (translationObject .: "translation" :: AET.Parser AET.Object)
    )


instance FromJSON TranslationContent where
    parseJSON (AE.Object obj) =
        TranslationContent <$> Map.fromList <$> fmap (\(x, y) -> (Text.unpack x, y)) <$> SHM.toList <$> Traversable.mapM translation obj
    parseJSON _ = mzero

getFiles :: Credential -> ProjectId -> [String] -> IO [FileTranslation]
getFiles credential projectId = mapConcurrently (getFile credential projectId)

getFile :: Credential -> ProjectId -> String -> IO FileTranslation
getFile (Credential apiKey secret) (ProjectId projectId) fileName = do
    (devHash, timestamp) <- fmap (getDevHash secret) getCurrentTimestamp
    resposne             <-
        httpJSON (getRequest devHash timestamp) :: IO
            (Response TranslationContent)
    return $ (FileTranslation fileName (getResponseBody resposne))
  where
    requestUrl =
        apiBaseUrl <> "/projects/" <> projectId <> "/translations/multilingual"
        -- "http://localhost:3000/translations"
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
