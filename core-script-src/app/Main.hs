#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Exception
import           Control.Monad.Reader      (runReaderT)
import           Data.List                 (isInfixOf, sort)
import qualified Data.Text                 as T
import           Distribution.System
import           Filesystem.Path
import           Filesystem.Path.CurrentOS (decodeString, encodeString,
                                            fromText)

import           Turtle

import           Context
import           DBConfig
import           DirSetup
import           Interactive
import           Lib
import           Run
import           StackBuild

data ExecutionContext = New Text | Run RunCmd

data RunCmd = RunAPI | RunWeb | RunMigrations

parseCmd :: Parser ExecutionContext
parseCmd =
      fmap New (subcommand "new" "Setup new App" $ argText "appName" "Choose a name for your app")
  <|> fmap Run (subcommand "start" "Run services" parseRunCmd)

parseRunCmd :: Parser RunCmd
parseRunCmd =
  arg parseRunText "runCmd" "Choose either 'front-end', 'back-end', or 'migrations'"
  where
    parseRunText rt =
      case rt of
        "back-end"   -> Just RunAPI
        "front-end"  -> Just RunWeb
        "migrations" -> Just RunMigrations
        _            -> Nothing


parser :: Parser (ExecutionContext, Maybe Text, Maybe Text)
parser = (,,)
             <$> parseCmd
             <*> optional (optText "front-end"  'a' "Choice of front-end")
             <*> optional (optText "template"  'a' "Choice of template")

main :: IO ()
main = do
  preValidate

  (executionArg, mbfrontEndOption, mbTemplate) <- options "Options" parser
  case executionArg of
    New appName   -> setupNew appName mbfrontEndOption mbTemplate
    Run runOption -> runCmd runOption

runCmd :: RunCmd -> IO ()
runCmd runOption = do
  isValidHASMDir <- checkValidHASMDir
  if isValidHASMDir then do
    appRootDir <- pwd
    executablePath <- getExecutablePath'
    let context = Context appRootDir executablePath buildOS Nothing
    io runCmdInContext context
  else
    ioError $ userError "You are not in a valid HASM directory"
  return ()
  where
    runCmdInContext =
      case runOption of
        RunAPI        -> runBackEnd
        RunWeb        -> runFrontEnd
        RunMigrations -> runMigrations


setupNew :: Text -> Maybe Text -> Maybe Text -> IO ()
setupNew appNameOption mbFrontEndOption mbTemplate = do
  curDir <- pwd
  let curOS = buildOS
      appDir = curDir </> fromText appNameOption
  executablePath <- getExecutablePath'
  majorCommentBlock "INITIAL SETUP"
  let dbConfig = mkDBConfig appNameOption
  showDBInfo dbConfig

  mkdir appDir
  let context = Context appDir executablePath curOS mbTemplate

  -- | Setup Ops, DB, Front-End, Back-End directories
  io (setupAllSubDirectories dbConfig) context

  shouldBuild <- askToBuild
  if shouldBuild then do
    io (buildFrontAndBackend dbConfig) context
  else
    return ()

  showRunInstructions

  cd appDir
  return ()

preValidate :: IO ()
preValidate = do
  validateDockerInstall <- shell "which docker" Turtle.empty
  case validateDockerInstall of
    ExitSuccess   -> return ()
    ExitFailure n -> die ("Failed to detect docker in system. Please install docker first before running haskstar: " <> repr n)
