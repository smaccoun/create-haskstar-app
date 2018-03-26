#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Turtle
import Filesystem.Path.CurrentOS (fromText, encodeString)
import System.Environment (getExecutablePath)
import Filesystem.Path
import qualified Data.Text as T

parser :: Parser (Text, Maybe Text)
parser = (,) <$> argText "app-name" "Name of directory to put your app in"
             <*> optional (optText "front-end"  'a' "Choise of front-end")

main :: IO ()
main = do
  (appNameOption, mbfrontEndOption) <- options "Options" parser
  executablePath <- getExecutablePath
  curDir <- pwd
  let appPath = curDir </> fromText (appNameOption)
  mkdir appPath
  let executablePathT = executablePath & T.pack & fromText
  let runOps =  (parent executablePathT) </> "ops"
  putStrLn $ encodeString runOps
  chmod executable (runOps </> "ttab")
  echo "Meow"
  cptree runOps appPath
  cd appPath
  setupDir appPath backendDirConfig
  setupDir appPath frontendDirConfig
  askToRun $ runServers appPath
  cd appPath
  return ()
