#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Run where

import qualified Configuration.Dotenv       as Dotenv
import qualified Configuration.Dotenv.Types as DET
import           Context
import qualified Control.Foldl              as Fold
import           Data.Text                  (pack)
import           DBConfig                   (DBConfig, PGConfig (..), port)
import           Filesystem.Path.CurrentOS  (encodeString)
import           Interactive
import           System.Envy
import           Turtle

runFrontEnd :: ScriptRunContext ()
runFrontEnd = do
  liftIO $ majorCommentBlock "STARTING WEB SERVER"
  frontendDir <- getFrontendDir
  cd frontendDir
  let runCmd = "yarn start"
  s <- shell runCmd empty
  case s of
    ExitSuccess -> do
        liftIO $ printf ("\nSuccessfully started server. Go to localhost:3000\n")
        fromAppRootDir
        return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)


runBackEnd :: ScriptRunContext ()
runBackEnd = do
  liftIO $ majorCommentBlock "STARTING LOCAL BACK-END"
  backendDir <- getBackendDir
  cd backendDir
  s <- shell "./run.sh" empty
  case s of
    ExitSuccess   -> do
        printf "\nSuccessfully started api. Logs will be output to console\n"
        fromAppRootDir
        return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)
  return ()

dockerRunDBCmd :: PGConfig -> Text
dockerRunDBCmd pgConfig =
  pack $ "docker run --name my-app-db  -p " <> portBind <> " -h 127.0.0.1 --env-file .env -d postgres"
  where
    portText = (show $ postgresPort pgConfig)
    portBind = portText <> ":5432"

getPGConfig :: ScriptRunContext PGConfig
getPGConfig = do
  fromAppRootDir
  cd "db"
  Dotenv.loadFile DET.defaultConfig
  pgConfigRead <- liftIO decodeEnv
  liftIO $ putStrLn (show pgConfigRead)
  case pgConfigRead of
    Left e    -> liftIO $ failCase e
    Right pgc -> return pgc
  where
    failCase e = die $ "Couldn't read .env for PG Config " <> pack e

dbLoginCmd :: PGConfig -> Text
dbLoginCmd (PGConfig pgDB pgUser pgPassword pgPort) =
   "psql -d " <> pgDB <> " -U " <> pgUser <> " --password -h localhost  -p " <> (show pgPort & pack)

loginDB :: ScriptRunContext ()
loginDB = do
  pgConfig <- getPGConfig
  liftIO $ print $ dbLoginCmd pgConfig
  _ <- shell (dbLoginCmd pgConfig) empty
  return ()


runDB :: ScriptRunContext ()
runDB = do
  backendDir <- getBackendDir
  pgConfig <- getPGConfig
  let dockerRunCmd = dockerRunDBCmd pgConfig
  liftIO $ majorCommentBlock $ "STARTING DB " <> dockerRunCmd
  dockerRunResult <- shell dockerRunCmd empty
  case dockerRunResult of
    ExitSuccess   -> do
        liftIO $ instructionCommentBlock $ "\nSuccessfully booted docker instance.\n To log into the database run:" <> dbLoginCmd pgConfig
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
