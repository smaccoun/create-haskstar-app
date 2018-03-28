#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Main where


import Turtle
import Filesystem.Path.CurrentOS (fromText, encodeString)
import System.Environment (getExecutablePath)
import Filesystem.Path
import qualified Data.Text as T

import Lib
import Interactive

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
      runOps =  (parent executablePathT) </> "ops"
  putStrLn $ encodeString runOps
  chmod executable (runOps </> "ttab")
  cptree runOps appPath

  _ <- shell "cat logoAscii.txt" Turtle.empty
  cd appPath
  majorCommentBlock "INITIAL SETUP"
  dbConfig <- getDBConfig

  majorCommentBlock "BACK-END"
  setupDir dbConfig appPath backendDirConfig
  majorCommentBlock "DB"
  setupDBDir dbConfig appPath
  majorCommentBlock "FRONT-END"
  setupDir dbConfig appPath frontendDirConfig


  askToRun $ runServers appPath
  cd appPath
  return ()
