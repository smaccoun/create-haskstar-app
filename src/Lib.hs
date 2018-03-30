#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Turtle
import Filesystem.Path.CurrentOS (encodeString)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Interactive
import Context
---------------------------------------------------------------

data DirSetup =
  DirSetup
    {dirStackType :: DirStackType
    ,dirName :: Text
    ,gitDir :: Text
    }

data DirStackType = FRONT_END | BACK_END

frontendDirConfig :: DirSetup
frontendDirConfig =
  DirSetup
      {dirStackType = FRONT_END
      ,dirName = "front-end"
      ,gitDir = "git@github.com:smaccoun/haskstar-elm.git"
      }

backendDirConfig :: DirSetup
backendDirConfig =
  DirSetup
      {dirStackType = BACK_END
      ,dirName = "back-end"
      ,gitDir = "git@github.com:smaccoun/haskstar-haskell.git"
      }

asLocal :: Text -> Text
asLocal dirName = "./" <> dirName


getDir :: Turtle.FilePath -> DirSetup -> (Text, Turtle.FilePath)
getDir rootDir dirSetup =
  (dname, dPath)
  where
    dname = dirName dirSetup
    dPath = rootDir </> (fromText dname)


setupDir :: DBConfig -> DirSetup -> App ()
setupDir dbConfig dirSetup = do
  appRootDir' <- getAppRootDir
  let (dname, dPath) = getDir appRootDir' dirSetup
  getTemplate dname dirSetup
  case dirStackType dirSetup of
    FRONT_END -> buildFrontEnd
    BACK_END -> buildBackEnd dbConfig


getTemplate :: Text -> DirSetup -> App ()
getTemplate dname dirSetup = do
  liftIO $ subCommentBlock $ "Setting up " <> dname
  fromAppRootDir
  setupResult <- shell ("git clone " <> gitDir dirSetup <> " " <> dname) empty
  case setupResult of
    ExitSuccess   -> return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)

buildFrontEnd :: App ()
buildFrontEnd = do
  topDir <- getAppRootDir
  liftIO $ subCommentBlock "Building front-end"
  let frontEndPath = getDir topDir frontendDirConfig & snd
  liftIO $ putStrLn $ encodeString frontEndPath
  cd frontEndPath
  _ <- shell "yarn install" empty
  _ <- shell "elm-package install --yes" empty
  return ()

buildBackEnd :: DBConfig -> App ()
buildBackEnd dbConfig = do
  liftIO $ subCommentBlock "Building back-end"
  fromAppRootDir
  topDir <- getAppRootDir
  let backendDir = getDir topDir backendDirConfig & snd
  cd backendDir
  _ <- shell "stack build" empty
  liftIO $ majorCommentBlock "SETUP DB CONFIGURATION"
  _ <- liftIO $ mkBackendEnv dbConfig backendDir
  return ()

data DBConfig =
  DBConfig
    {host :: T.Text
    ,port :: Int
    ,dbName :: T.Text
    ,dbUser :: T.Text
    ,dbPassword :: T.Text
    ,dbSchema :: T.Text
    }

getDBConfig :: IO DBConfig
getDBConfig = do
  instructionCommentBlock "First setup the DB configuration you would like"
  dbName <- prompt "Enter name of DB" Nothing
  dbSchema <- prompt "Enter default schema" (Just "public")
  dbUser <- prompt "Enter name of User" (Just "postgres")
  dbPassword <- prompt "Enter DB Password" (Just "postgres")
  return $
    DBConfig
      {host = "localhost"
      ,port = 5432
      ,dbName = dbName
      ,dbUser = dbUser
      ,dbPassword = dbPassword
      ,dbSchema = dbSchema
      }

setupDBDir :: DBConfig -> App ()
setupDBDir dbConfig = do
  liftIO $ majorCommentBlock "SETTING UP DB"
  rootDir <- getAppRootDir
  fromAppRootDir
  mkdir "db"
  templateDir' <- getTemplateDir
  let simpleMigrationDir = templateDir' </> fromText "db" </> fromText "migrations" </> fromText "haskell" </> fromText "pg-simple"
  cptree simpleMigrationDir "./db"
  cd "./db"
  let dbEnvFile = getDBEnvFile dbConfig rootDir
  liftIO $ writeTextFile ".env" dbEnvFile
  dockerRunResult <- shell dockerRunCmd empty
  case dockerRunResult of
    ExitSuccess   -> do
        liftIO $ instructionCommentBlock $ "\nSuccessfully booted docker instance.\n To log into the database run:" <> dockerRunCmd
        _ <- shell "stack build" empty
        _ <- shell "./run.sh" empty
        cd rootDir
        return ()
    ExitFailure n -> die ("Failed to boot docker instance for DB: " <> repr n)
  where
    dockerRunCmd = "docker run --name my-app-db  -p 5432:5432 -h 127.0.0.1 --env-file .env -d postgres"


getDBEnvFile :: DBConfig -> Turtle.FilePath -> T.Text
getDBEnvFile (DBConfig host port dbName dbUser dbPassword dbSchema) backendDir =
  T.intercalate "\n" $
         [ dbDatabaseLn dbName
         , dbUserLn dbUser
         , dbPasswordLn dbPassword
         ]
  where
    dbPasswordLn password = "POSTGRES_PASSWORD=" <> dbPassword
    dbDatabaseLn dbName   = "POSTGRES_DB=" <> dbName
    dbUserLn dbUser = "POSTGRES_USER=" <> dbUser


mkBackendEnv :: DBConfig -> Turtle.FilePath -> IO ()
mkBackendEnv (DBConfig host port dbName dbUser dbPassword dbSchema) backendDir = do
  let textFile = T.intercalate "\n" $
         [ dbHostLn
         , dbPortLn
         , dbDatabaseLn dbName
         , dbSchemaLn dbSchema
         , dbUserLn dbUser
         , dbPasswordLn dbPassword
         ]
  writeTextFile (backendDir </> ".env") textFile

  where
    dbHostLn    = "DB_HOST=localhost"
    dbPortLn    = "DB_PORT=5432"
    dbDatabaseLn dbName   = "DB_DATABASE=" <> dbName
    dbSchemaLn schema     = "DB_SCHEMA=" <> schema
    dbUserLn dbUser = "DB_USERNAME=" <> dbUser
    dbPasswordLn password = "DB_PASSWORD=" <> dbPassword



