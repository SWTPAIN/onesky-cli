{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe                     ( fromMaybe )
import           Lib
import           System.Environment
import           Turtle
import           Data.Text                     as Text
import           Paths_oneup_cli                ( version )
import           Data.Version                   ( showVersion )
import           Network.Wreq
import           Control.Lens
import qualified OneSkyApi

-- return apiAddress + '/1/projects/' + options.projectId + '/translations/multilingual?' + queryString.stringify({
--   api_key: options.apiKey,
--   timestamp: options.hash.timestamp,
--   dev_hash: options.hash.devHash,
--   source_file_name: options.fileName,
--   file_format: options.format || 'I18NEXT_MULTILINGUAL_JSON'
-- });

version' :: IO ()
version' = putStrLn (showVersion version)

parseVersion :: Turtle.Parser (IO ())
parseVersion =
  (subcommand "version" "Show the Awesome tool version" (pure version'))

parseDownload :: Turtle.Parser (IO ())
parseDownload = fmap
  downloadTranslation
  (subcommand "download" "Download Translation from Onesky API" downloadArgs)

downloadArgs :: Turtle.Parser (Turtle.Text, Bool, Maybe Turtle.Text)
downloadArgs =
  (,,)
    <$> (optText "directory" 'd' "Directory for putting downloaded translation "
        )
    <*> (switch "all" 'a' "Is Downloading all languages")
    <*> optional
          (optText "language" 'l' "The language of translation to be downloaded"
          )

downloadTranslation :: (Turtle.Text, Bool, Maybe Turtle.Text) -> IO ()
downloadTranslation (directory, True, Nothing) =
  OneSkyApi.getFiles (OneSkyApi.Credential "apiKey" "secret")
                     (Text.unpack directory)
    >>= putStrLn
downloadTranslation (directory, True, Just _) =
  putStrLn "You can only either specific one lang or all langauges"
downloadTranslation (directory, False, Just lang) =
  putStrLn "downloading one translation"
downloadTranslation (directory, False, Nothing) =
  putStrLn "Please speciifc one language or all langauge"

parser :: Turtle.Parser (IO ())
parser = parseVersion <|> parseDownload

main :: IO ()
main = do
  config <- readEnv
  join
    (Turtle.options "A command-line tool for managing translation in Onesky"
                    parser
    )
