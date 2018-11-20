{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe                     ( fromMaybe )
import           Lib
import           System.Environment
import           Turtle
import           Paths_oneup_cli                ( version )
import           Data.Version                   ( showVersion )

version' :: IO ()
version' = putStrLn (showVersion version)

parseVersion :: Parser (IO ())
parseVersion =
  (subcommand "version" "Show the Awesome tool version" (pure version'))

parseDownload :: Parser (IO ())
parseDownload = fmap
  downloadTranslation
  (subcommand "download" "Download Translation from Onesky API" downloadArgs)

downloadArgs :: Parser (Text, Bool, Maybe Text)
downloadArgs =
  (,,)
    <$> (optText "directory" 'd' "Directory for putting downloaded translation "
        )
    <*> (switch "all" 'a' "Is Downloading all languages")
    <*> optional
          (optText "language" 'l' "The language of translation to be downloaded"
          )

downloadTranslation :: (Text, Bool, Maybe Text) -> IO ()
downloadTranslation (directory, True, Nothing) =
  putStrLn "downloading all translation"
downloadTranslation (directory, True, Just _) =
  putStrLn "You can only either specific one lang or all langauges"
downloadTranslation (directory, False, Just lang) =
  putStrLn "downloading one translation"
downloadTranslation (directory, False, Nothing) =
  putStrLn "Please speciifc one language or all langauge"

parser :: Parser (IO ())
parser = parseVersion <|> parseDownload

main :: IO ()
main = do
  config <- readEnv
  join (options "A command-line tool for managing translation in Onesky" parser)
