{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe                     ( fromMaybe )
import           Lib
import           System.Environment

main :: IO ()
main = do
  config <- readEnv
  print config
