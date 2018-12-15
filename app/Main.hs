{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO                  as TextIO
import           Lib
import           System.Environment
import qualified Data.ByteString.Lazy          as LBS
import           Turtle
import           Data.Text                     as Text
import           Paths_oneup_cli                ( version )
import           Data.Version                   ( showVersion )
import qualified OneSkyApi
import           System.Exit                    ( exitFailure )
import qualified Data.Map                      as Map
import           Control.Monad                  ( filterM )
import           System.Directory               ( getDirectoryContents
                                                , doesDirectoryExist
                                                , createDirectoryIfMissing
                                                )

version' :: IO ()
version' = putStrLn (showVersion version)

parseVersion :: Turtle.Parser (IO ())
parseVersion =
  (subcommand "version" "Show the Awesome tool version" (pure version'))

parseDownload :: Config -> Turtle.Parser (IO ())
parseDownload config =
  fmap (downloadTranslations config)
    $ (subcommand "download" "Download Translation from Onesky API" downloadArgs
      )

parseUpload:: Config -> Turtle.Parser (IO ())
parseUpload config =
  fmap (uploadTranslations config)
    $ (subcommand "upload" "Upload Translation from Onesky API" uploadArgs
      )

uploadArgs :: Turtle.Parser (Turtle.Text, Bool, Maybe Turtle.Text)
uploadArgs =
  (,,)
    <$> (optText "directory" 'd' "Directory for translation to be uploaded "
        )
    <*> (switch "all" 'a' "Is Uploading all languages")
    <*> optional
          (optText "language" 'l' "The language of translation to be uploaded"
          )

downloadArgs :: Turtle.Parser (Turtle.Text, Bool, Maybe Turtle.Text)
downloadArgs =
  (,,)
    <$> (optText "directory" 'd' "Directory for putting downloaded translation "
        )
    <*> (switch "all" 'a' "Is Downloading all languages")
    <*> optional
          (optText "language" 'l' "The language of translation to be downloaded"
          )

downloadTranslations
  :: Config -> (Turtle.Text, Bool, Maybe Turtle.Text) -> IO ()
downloadTranslations _ (directory, True, Just _) =
  putStrLn "You can only either specific one lang or all langauges"
downloadTranslations _ (directory, False, Just lang) =
  putStrLn "downloading one translation"
downloadTranslations _ (directory, False, Nothing) =
  putStrLn "Please speciifc one language or all langauge"
downloadTranslations (Config oneskyProjectId oneskyApiKey oneskySecretKey) (directory, True, Nothing)
  = do
    fileNames    <- getFileNames sourceDirName
    translations <- OneSkyApi.getFiles
      (OneSkyApi.Credential oneskyApiKey oneskySecretKey)
      (OneSkyApi.ProjectId oneskyProjectId)
      fileNames
    mapM (writeTranslation dirName) translations
    putStrLn "Finish Downloading transactions"
 where
  dirName       = (Text.unpack directory)
  sourceDirName = dirName <> "/en"
  getFileNames dirName =
    getDirectoryContents dirName >>= filterM (fmap not . doesDirectoryExist)

uploadTranslations
  :: Config -> (Turtle.Text, Bool, Maybe Turtle.Text) -> IO ()
uploadTranslations _ (directory, True, Just _) =
  putStrLn "You can only either specific one lang or all langauges"
uploadTranslations _ (directory, False, Just lang) =
  putStrLn "uploading one translation"
uploadTranslations _ (directory, False, Nothing) =
  putStrLn "Please speciifc one language or all langauge"
uploadTranslations (Config oneskyProjectId oneskyApiKey oneskySecretKey) (directory, True, Nothing)
  = do
    fileNames    <- getFileNames sourceDirName
    translations <- OneSkyApi.getFiles
      (OneSkyApi.Credential oneskyApiKey oneskySecretKey)
      (OneSkyApi.ProjectId oneskyProjectId)
      fileNames
    mapM (writeTranslation dirName) translations
    putStrLn "Finish Downloading transactions"
  where
  dirName       = (Text.unpack directory)
  sourceDirName = dirName <> "/en"
  getFileNames dirName =
    getDirectoryContents dirName >>= filterM (fmap not . doesDirectoryExist)

writeTranslation :: String -> OneSkyApi.FileTranslation -> IO ()
writeTranslation dirName (OneSkyApi.FileTranslation fileName (OneSkyApi.TranslationContent translationContent))
  = mapM_ (writeLangTranslation dirName fileName)
          (Map.toList translationContent)

writeLangTranslation :: String -> String -> (String, Text) -> IO ()
writeLangTranslation targetDir filename (lang, translation) = do
  createDirectoryIfMissing False directory
  TextIO.writeFile filePath translation
  where
    directory = targetDir <> "/" <> lang
    filePath = directory <> "/" <> filename

parser :: Config -> Turtle.Parser (IO ())
parser config = parseVersion <|> parseDownload <|> parseUpload config

main :: IO ()
main = do
  mconfig <- readEnv
  case mconfig of
    Nothing     -> putStrLn "Cannot read config." >> exitFailure
    Just config -> join
      ( Turtle.options "A command-line tool for managing translation in Onesky"
      $ parser config
      )
