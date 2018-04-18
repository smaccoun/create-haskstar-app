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

data ExecutionContext = New Text | PostSetupMode PostSetupOption

data PostSetupOption = Build BuildCmd | Start StartCmd | Run RunCmd

data BuildCmd = BuildFrontEnd | BuildBackEnd | BuildAll
data StartCmd = StartAPI | StartWeb | StartDB
data RunCmd = RunMigrations

parseCmd :: Parser ExecutionContext
parseCmd =
      fmap New (subcommand "new" "Setup new App" $ argText "appName" "Choose a name for your app")
  <|> fmap (\a -> PostSetupMode (Start a))
        (subcommand "start" "Start services" parseStartCmd)
  <|> fmap (\a -> PostSetupMode (Run a))
        (subcommand "run" "Run services" parseRunCmd)
  <|> fmap (\a -> PostSetupMode (Build a))
        (subcommand "build" "Build services" parseBuildCmd)

parseStartCmd :: Parser StartCmd
parseStartCmd =
  arg parseStartText "startCmd" "Choose either 'front-end', 'back-end', or 'db'"
  where
    parseStartText rt =
      case rt of
        "back-end"  -> Just StartAPI
        "front-end" -> Just StartWeb
        "db"        -> Just StartDB
        _           -> Nothing

parseRunCmd :: Parser RunCmd
parseRunCmd =
  arg parseStartText "runCmd" "Choose 'migrations'"
  where
    parseStartText rt =
      case rt of
        "migrations" -> Just RunMigrations
        _            -> Nothing

parseBuildCmd :: Parser BuildCmd
parseBuildCmd =
  arg parseStartText "buildCmd" "Choose either 'front-end', 'back-end', or 'migrations'"
  where
    parseStartText rt =
      case rt of
        "back-end"  -> Just BuildFrontEnd
        "front-end" -> Just BuildBackEnd
        "all"       -> Just BuildAll
        _           -> Nothing


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
    PostSetupMode postSetupOption -> do
      context <- getPostSetupContext
      case postSetupOption of
        Start runOption   -> startCmd context runOption
        Run runOption     -> runCmd context runOption
        Build buildOption -> buildCmd context buildOption


buildCmd :: Context -> BuildCmd -> IO ()
buildCmd context buildOption = do
  validateAndRunPostSetupCmd context buildCmdInContext
  return ()
  where
    buildCmdInContext =
      case buildOption of
        BuildFrontEnd -> buildFrontEnd
        BuildBackEnd  -> buildBackEnd
        BuildAll      -> buildFrontAndBackEnd


runCmd :: Context -> RunCmd -> IO ()
runCmd context RunMigrations =
  io runMigrations context

startCmd :: Context -> StartCmd -> IO ()
startCmd context runOption = do
  validateAndRunPostSetupCmd context runCmdInContext
  return ()
  where
    runCmdInContext =
      case runOption of
        StartAPI -> runBackEnd
        StartWeb -> runFrontEnd
        StartDB  -> runDB


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
    io buildFrontAndBackEnd context
  else
    return ()

  showRunInstructions

  cd appDir
  exit ExitSuccess

preValidate :: IO ()
preValidate = do
  validateDockerInstall <- shell "which docker" Turtle.empty
  case validateDockerInstall of
    ExitSuccess   -> return ()
    ExitFailure n -> die ("Failed to detect docker in system. Please install docker first before running haskstar: " <> repr n)
