#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Main where


import           Control.Exception
import           Control.Monad.Reader      (runReaderT)
import           Data.List                 (isInfixOf, sort)
import qualified Data.Text                 as T
import           Filesystem.Path
import           Filesystem.Path.CurrentOS (decodeString, encodeString,
                                            fromText)

import           Turtle

import           Build
import           Context
import           DBConfig
import           DirSetup
import           Interactive
import           Lib
import           Options
import           PostSetup.Config
import           PostSetup.Context
import           PostSetup.Deploy
import           PostSetup.Run
import           Run


main :: IO ()
main = do
  preValidate

  (executionArg, mbfrontEndOption, mbTemplate) <- options "Options" parser
  case executionArg of
    New appName   -> setupNew appName mbfrontEndOption mbTemplate
    PostSetupMode postSetupOption -> do
      context <- getPostSetupContext
      validateAndRunPostSetupCmd context (runPostSetupOption postSetupOption)


runPostSetupOption :: PostSetupOption -> ScriptRunContext ()
runPostSetupOption postSetupOption =
  case postSetupOption of
    Start startOption             -> startCmd startOption
    Run runOption                 -> runCmd runOption
    Build buildOption             -> buildCmd buildOption
    Deploy deployConfig deployEnv ->  deployCmd deployConfig deployEnv
    Configure env cf              ->   configureCmd env cf
    Login loginCmd                -> loginDB


buildCmd :: BuildCmd -> ScriptRunContext ()
buildCmd buildOption =
  case buildOption of
    BuildFrontEnd -> buildFrontEnd
    BuildBackEnd  -> buildBackEnd
    BuildAll      -> buildFrontAndBackEnd

runCmd :: RunCmd -> ScriptRunContext ()
runCmd (RunMigrations migrationEnv) =
  runMigrations migrationEnv

startCmd :: StartCmd -> ScriptRunContext ()
startCmd runOption =
  case runOption of
    StartAPI -> runBackEnd
    StartWeb -> runFrontEnd
    StartDB  -> runDB


deployCmd :: DeployConfig -> RemoteStage -> ScriptRunContext ()
deployCmd deployConfig deployEnv =
  case deployEnv of
    Staging    -> deploy deployConfig
    Production -> deploy deployConfig


configureCmd :: Environment -> ConfigureCmd -> ScriptRunContext ()
configureCmd env configCmd = do
  case configCmd of
    ConfigureDB -> do
      interactiveConfigureDB env

setupNew :: Text -> Maybe Text -> Maybe Text -> IO ()
setupNew appNameOption mbFrontEndOption mbTemplate = do
  curDir <- pwd
  let appDir = curDir </> fromText appNameOption
  executablePath <- getExecutablePath'
  majorCommentBlock "INITIAL SETUP"
  let dbConfig = mkDefaultLocalDBConfig appNameOption
  showDBInfo dbConfig

  mkdir appDir

  let context = Context appDir executablePath mbTemplate

  -- | Setup Ops, DB, Front-End, Back-End directories
  io (runSetup (AppName appNameOption) dbConfig) context

  shouldBuild <- askToBuild
  if shouldBuild then do
    io buildBackEnd context
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
