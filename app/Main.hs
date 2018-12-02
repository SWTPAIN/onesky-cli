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

version' :: IO ()
version' = putStrLn (showVersion version)

parseVersion :: Turtle.Parser (IO ())
parseVersion =
  (subcommand "version" "Show the Awesome tool version" (pure version'))

parseDownload :: Config -> Turtle.Parser (IO ())
parseDownload config =
  fmap (downloadTranslation config)
    $ (subcommand "download" "Download Translation from Onesky API" downloadArgs
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

downloadTranslation :: Config -> (Turtle.Text, Bool, Maybe Turtle.Text) -> IO ()
downloadTranslation (Config oneskyProjectId oneskyApiKey oneskySecretKey) (directory, True, Nothing)
  = do
    OneSkyApi.Translations translations <- OneSkyApi.getFiles
      (OneSkyApi.Credential oneskyApiKey oneskySecretKey)
      (OneSkyApi.ProjectId oneskyProjectId)
      (Text.unpack directory)
    mapM_ writeTranslation (Map.toList translations)

downloadTranslation _ (directory, True, Just _) =
  putStrLn "You can only either specific one lang or all langauges"
downloadTranslation _ (directory, False, Just lang) =
  putStrLn "downloading one translation"
downloadTranslation _ (directory, False, Nothing) =
  putStrLn "Please speciifc one language or all langauge"

writeTranslation :: (String, Text) -> IO ()
writeTranslation (lang, translation) = TextIO.writeFile filename translation
  where filename = lang <> ".json"

parser :: Config -> Turtle.Parser (IO ())
parser config = parseVersion <|> parseDownload config

main :: IO ()
main = do
  mconfig <- readEnv
  case mconfig of
    Nothing     -> putStrLn "Cannot create config." >> exitFailure
    Just config -> join
      ( Turtle.options "A command-line tool for managing translation in Onesky"
      $ parser config
      )
