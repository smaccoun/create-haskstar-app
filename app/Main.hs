#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad.Reader      (runReaderT)
import qualified Data.Text                 as T
import           Distribution.System
import           Filesystem.Path
import           Filesystem.Path.CurrentOS (encodeString, fromText)
import           System.Environment        (getExecutablePath)
import           Turtle

import           Context
import           Interactive
import           Lib
import           Run

parser :: Parser (Text, Maybe Text)
parser = (,) <$> argText "app-name" "Name of directory to put your app in"
             <*> optional (optText "front-end"  'a' "Choise of front-end")

main :: IO ()
main = do
  preValidate
  let curOS = buildOS
  (appNameOption, mbfrontEndOption) <- options "Options" parser
  executablePath <- getExecutablePath
  curDir <- pwd
  let appPath = curDir </> fromText (appNameOption)
  mkdir appPath
  let executablePathT = executablePath & T.pack & fromText
      opsDir' =  (parent executablePathT) </> "ops"

  let context = Context appPath executablePathT opsDir' curOS
  chmod executable (opsDir' </> "ttab")
  cptree opsDir' appPath

  _ <- shell "cat logoAscii.txt" Turtle.empty
  cd appPath
  majorCommentBlock "INITIAL SETUP"
  dbConfig <- getDBConfig

  runReaderT (setupAndRunDirectories dbConfig) context

  cd appPath
  return ()

setupAndRunDirectories :: DBConfig -> App ()
setupAndRunDirectories dbConfig = do
  (setupAllSubDirectories dbConfig)
  (askToRun runServers)

setupAllSubDirectories :: DBConfig -> App ()
setupAllSubDirectories dbConfig = do
  appPath <- getAppRootDir
  liftIO $ majorCommentBlock "BACK-END"
  setupDir dbConfig backendDirConfig
  liftIO $ majorCommentBlock "DB"
  setupDBDir dbConfig
  liftIO $ majorCommentBlock "FRONT-END"
  setupDir dbConfig frontendDirConfig


preValidate :: IO ()
preValidate = do
  validateDockerInstall <- shell "which docker" Turtle.empty
  case validateDockerInstall of
    ExitSuccess   -> return ()
    ExitFailure n -> die ("Failed to detect docker in system. Please install docker first before running haskstar: " <> repr n)
