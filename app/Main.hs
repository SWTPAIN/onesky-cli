{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Data.Encoding.UTF8
-- import           Data.ByteString                ( writeFile )
import           Prelude                 hiding ( readFile
                                                , writeFile
                                                )
import           Data.Text.IO                   ( writeFile )
import qualified Data.Text.Encoding            as TextEncoding
import qualified Data.Text.IO                  as TextIO
import           Data.Maybe                     ( fromMaybe )
import           Lib
import           System.Environment
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as LBS
import           Turtle
import           Data.Text                     as Text
import           Paths_oneup_cli                ( version )
import           Data.Version                   ( showVersion )
import qualified OneSkyApi
import           System.Exit                    ( exitFailure )

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
    files <- OneSkyApi.getFiles
      (OneSkyApi.Credential oneskyApiKey oneskySecretKey)
      (OneSkyApi.ProjectId oneskyProjectId)
      (Text.unpack directory)
    LBS.writeFile "filename.json" files
    return ()

downloadTranslation _ (directory, True, Just _) =
  putStrLn "You can only either specific one lang or all langauges"
downloadTranslation _ (directory, False, Just lang) =
  putStrLn "downloading one translation"
downloadTranslation _ (directory, False, Nothing) =
  putStrLn "Please speciifc one language or all langauge"

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
