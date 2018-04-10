#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Monad.Reader      (runReaderT)
import qualified Data.Text                 as T
import           Distribution.System
import           Filesystem.Path
import           Filesystem.Path.CurrentOS (decodeString, encodeString,
                                            fromText)
import           System.Environment        (getExecutablePath)
import           Turtle

import           Context
import           DBConfig
import           DirSetup
import           Interactive
import           Lib
import           Run
import           StackBuild

parser :: Parser (Text, Maybe Text, Maybe Text)
parser = (,,)
             <$> argText "app-name" "Name of directory to put your app in"
             <*> optional (optText "front-end"  'a' "Choice of front-end")
             <*> optional (optText "template"  'a' "Choice of template")

main :: IO ()
main = do
  preValidate

  (appNameOption, mbfrontEndOption, mbTemplate) <- options "Options" parser
  curDir <- pwd
  let curOS = buildOS
      runEnv = Development
      appDir = curDir </> fromText (appNameOption)
  executablePath <- fmap (ExecutablePath . decodeString) getExecutablePath
  majorCommentBlock "INITIAL SETUP"
  let dbConfig = mkDBConfig appNameOption
  showDBInfo dbConfig

  mkdir appDir
  let context = setContext runEnv appDir executablePath curOS mbTemplate

  -- | Setup Ops, DB, Front-End, Back-End directories
  io (setupAllSubDirectories dbConfig) context

  shouldBuild <- askToBuild
  if shouldBuild then do
    io (buildFrontAndBackend dbConfig) context
    io (askToRun runServers) context
  else
    echo "Complete! Please follow the docs for running your application"

  cd appDir
  return ()

preValidate :: IO ()
preValidate = do
  validateDockerInstall <- shell "which docker" Turtle.empty
  case validateDockerInstall of
    ExitSuccess   -> return ()
    ExitFailure n -> die ("Failed to detect docker in system. Please install docker first before running haskstar: " <> repr n)
