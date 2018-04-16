#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Run where

import           Context
import           Data.Text                 (pack)
import           DBConfig                  (DBConfig, port)
import           Distribution.System
import           Filesystem.Path.CurrentOS (encodeString)
import           Interactive
import           Turtle

runFrontEnd :: ScriptRunContext ()
runFrontEnd = do
  liftIO $ majorCommentBlock "STARTING WEB SERVER"
  fromAppRootDir
  curOS' <- getCurOS
  cd "front-end"
  let runCmd = "yarn start"
  s <-
    case curOS' of
      OSX -> runWithTTab runCmd
      _   -> runAsBackground runCmd
  case s of
    ExitSuccess -> do
        liftIO $ printf ("\nSuccessfully started server. Go to localhost:3000\n")
        fromAppRootDir
        return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)


runBackEnd :: ScriptRunContext ()
runBackEnd = do
  liftIO $ majorCommentBlock "STARTING LOCAL BACK-END"
  fromAppRootDir
  cd "back-end"
  s <- runWithTTab "./run.sh"
  case s of
    ExitSuccess   -> do
        printf "\nSuccessfully started api. Logs will be output to console\n"
        fromAppRootDir
        return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)
  return ()

dockerRunDBCmd :: Text
dockerRunDBCmd =
  pack $ "docker run --name my-app-db  -p " <> portBind <> " -h 127.0.0.1 --env-file .env -d postgres"
  where
    portBind =  (show $ "6543") <> ":5432"


runDB :: ScriptRunContext ()
runDB = do
  liftIO $ majorCommentBlock "STARTING DB"
  fromAppRootDir
  cd "db"
  let dockerRunCmd = dockerRunDBCmd
  dockerRunResult <- shell dockerRunCmd empty
  case dockerRunResult of
    ExitSuccess   -> do
        liftIO $ instructionCommentBlock $ "\nSuccessfully booted docker instance.\n To log into the database run:" <> dockerRunCmd
        _ <- shell "stack build" empty
        _ <- shell "./run.sh" empty
        fromAppRootDir
        return ()
    ExitFailure n -> die ("Failed to boot docker instance for DB: " <> repr n)

runMigrations :: ScriptRunContext ()
runMigrations = do
  liftIO $ majorCommentBlock "RUNNING INITIAL MIGRATIONS"
  fromAppRootDir
  cd "db"
  s <- shell "./run.sh" empty
  case s of
    ExitSuccess   -> liftIO $ do
        printf "\nSuccessfully ran initial migration. \n"
        return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)
  return ()


runServers :: ScriptRunContext ()
runServers = do
    runMigrations
    runBackEnd
    runFrontEnd

askToRun :: ScriptRunContext () -> ScriptRunContext ()
askToRun onYes = do
  answer <- liftIO $ prompt "Setup Complete! Would you like to boot up the servers? (y) yes, (n) no" Nothing
  case answer of
     "y" -> onYes
     "n" -> liftIO $ echo "You can boot up each server by running ./run.sh"
     _   -> liftIO $ echo "Please entery (y) or (n)"


runWithTTab :: Text -> ScriptRunContext ExitCode
runWithTTab cmd = do
  ttabBaseCmd <- getTTab
  let asTextTTab = pack $ encodeString ttabBaseCmd
      ttabCmd =  asTextTTab <> " " <> cmd
  s <- liftIO $ shell ttabCmd empty
  return s

runAsBackground :: Text -> ScriptRunContext ExitCode
runAsBackground cmd = do
  let asBackground = cmd <> " &"
  s <- liftIO $ shell asBackground empty
  return s
