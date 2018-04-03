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
import           Interactive
import           Lib
import           Run

parser :: Parser (Text, Maybe Text)
parser = (,) <$> argText "app-name" "Name of directory to put your app in"
             <*> optional (optText "front-end"  'a' "Choice of front-end")

main :: IO ()
main = do
  preValidate
  let curOS = buildOS
  (appNameOption, mbfrontEndOption) <- options "Options" parser
  executablePath <- fmap (ExecutablePath . decodeString) getExecutablePath
  curDir <- pwd

  let runEnv = Development
      appPath = curDir </> fromText (appNameOption)
      parentExcutable = parent (unExecutablePath executablePath)
      templatesDir' = parentExcutable  </> "templates"

      opsDir' =  templatesDir' </> "ops"
      context = Context runEnv appPath executablePath opsDir' templatesDir' curOS

  putStrLn $ "TEMPLATES DIR" <> encodeString templatesDir'
  putStrLn $ "OPPS DIR" <> encodeString opsDir'
  mkdir appPath

  _ <- shell "cat logoAscii.txt" Turtle.empty
  let ttab = (opsDir' </> "ttab")
  putStrLn $ "TTAB: " <> encodeString ttab
  chmod executable ttab
  putStrLn $ "APP PATH: " <> encodeString appPath
  let appOpsDir = (appPath </> decodeString "ops")
  putStrLn $ "APP OPS DIR: " <> encodeString appOpsDir
  mkdir appOpsDir
  putStrLn $ "APP OPS DIR: " <> encodeString appOpsDir
  cptree opsDir' appOpsDir

  cd appPath
  majorCommentBlock "INITIAL SETUP"
  dbConfig <- getDBConfig

  io (setupAllSubDirectories dbConfig) context
  shouldBuild <- askToBuild
  if shouldBuild then do
    io (buildFrontAndBackend dbConfig) context
    io (askToRun runServers) context
  else
    echo "Complete! Please follow the docs for running your application"

  cd appPath
  return ()

io :: ScriptRunContext () -> Context -> IO ()
io action context =
    runReaderT action context

askToBuild :: IO Bool
askToBuild = do
  majorCommentBlock "Setup complete! You now have a fullstack Haskell setup!"
  answer <- prompt "Would you like to now build the project? (y) yes, (n) no" Nothing
  case answer of
     "y" -> return True
     "n" -> do
        echo "To build the back-end, cd into back-end and run `./run.sh`. For the front-end, cd into front-end and run `yarn start`"
        return False
     _   -> do
        echo "Please entery (y) or (n)"
        return False


setupAllSubDirectories :: DBConfig -> ScriptRunContext ()
setupAllSubDirectories dbConfig = do
  appPath <- getAppRootDir
  liftIO $ majorCommentBlock "DB"
  setupDBDir dbConfig
  liftIO $ majorCommentBlock "BACK-END"
  setupDir dbConfig backendDirConfig
  liftIO $ majorCommentBlock "FRONT-END"
  setupDir dbConfig frontendDirConfig


preValidate :: IO ()
preValidate = do
  validateDockerInstall <- shell "which docker" Turtle.empty
  case validateDockerInstall of
    ExitSuccess   -> return ()
    ExitFailure n -> die ("Failed to detect docker in system. Please install docker first before running haskstar: " <> repr n)
