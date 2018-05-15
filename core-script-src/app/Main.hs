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
import           Run


main :: IO ()
main = do
  preValidate

  (executionArg, mbfrontEndOption, mbTemplate) <- options "Options" parser
  case executionArg of
    New appName   -> setupNew appName mbfrontEndOption mbTemplate
    PostSetupMode postSetupOption -> do
      context <- getPostSetupContext
      case postSetupOption of
        Start startOption               -> startCmd context startOption
        Run runOption -> runCmd context runOption
        Build buildOption             -> buildCmd context buildOption
        Deploy deployConfig deployEnv ->  deployCmd context deployConfig deployEnv
        Login loginCmd -> io loginDB context



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
runCmd context (RunMigrations migrationEnv) =
  io (runMigrations migrationEnv) context

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

deployCmd :: Context -> DeployConfig -> RemoteStage -> IO ()
deployCmd context deployConfig deployEnv = do
  validateAndRunPostSetupCmd context deployInContext
  return ()
  where
    deployInContext =
      case deployEnv of
        Staging    -> deploy deployConfig
        Production -> deploy deployConfig


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
