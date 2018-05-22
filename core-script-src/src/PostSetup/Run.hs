#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module PostSetup.Run where

import qualified Configuration.Dotenv       as Dotenv
import qualified Configuration.Dotenv.Types as DET
import           Context
import qualified Control.Foldl              as Fold
import           Data.Text                  (pack)
import           DBConfig                   (DBConfig, PGConfig (..),
                                             dbConfigToPGConfig)
import           Filesystem.Path.CurrentOS  (encodeString)
import           Interactive
import           PostSetup.Config           (readDBConfig)
import           System.Envy
import           Turtle

runMigrations :: Environment -> ScriptRunContext ()
runMigrations curEnv = do
  liftIO $ majorCommentBlock "RUNNING INITIAL MIGRATIONS"
  fromAppRootDir
  cd "db"
  dbConfig <- readDBConfig curEnv
  s <- liftIO $ runMigrationWithConfig $ dbConfigToPGConfig dbConfig
  case s of
    ExitSuccess -> liftIO $ do
        printf "\nSuccessfully ran initial migration. \n"
        return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)
  return ()

runMigrationWithConfig :: PGConfig -> IO ExitCode
runMigrationWithConfig pgConfig = do
    _ <- setEnvironment' pgConfig
    execResult <- shell "stack exec pg-simple-exe" empty
    _ <- unsetEnvironment' pgConfig
    return execResult
