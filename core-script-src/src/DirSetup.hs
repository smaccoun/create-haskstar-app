#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module DirSetup where

import           Context
import           Data.Text                 (Text, intercalate, pack)
import qualified Data.Text                 as T
import           DBConfig
import           Filesystem.Path.CurrentOS (encodeString)
import           Interactive
import           Lib
import           Turtle

setupOpsDir :: ScriptRunContext ()
setupOpsDir = do
  fromAppRootDir
  liftIO $ do
    majorCommentBlock "Grabbing required templates"
    mktree "./ops/db"
    _ <- gitCloneShallow "git@github.com:smaccoun/create-haskstar-app.git"
    cptree "./create-haskstar-app/templates/ops" "./ops"
    cptree "./create-haskstar-app/templates/db" "./ops/db"
    rmtree "create-haskstar-app"

data DirSetup =
  DirSetup
    {dirStackType :: DirStackType
    ,dirName      :: Text
    ,gitDir       :: Text
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

getTemplate :: Turtle.FilePath -> DirSetup -> ScriptRunContext ()
getTemplate dPath dirSetup = do
  let dname = pack $ encodeString $ filename dPath
  liftIO $ subCommentBlock $ "Setting up " <> dname
  fromAppRootDir
  setupResult <- liftIO $ gitCloneShallow $ gitDir dirSetup <> " " <> dname
  liftIO $ rmtree (dPath </> fromText ".git")
  case setupResult of
    ExitSuccess   -> return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)


getDir :: Turtle.FilePath -> DirSetup -> (Text, Turtle.FilePath)
getDir rootDir dirSetup =
  (dname, dPath)
  where
    dname = dirName dirSetup
    dPath = rootDir </> (fromText dname)

-- | Setup DB, Front-End, Back-End directories without building them
setupCoreDirectories :: DBConfig -> ScriptRunContext ()
setupCoreDirectories dbConfig = do
  appDir <- getAppRootDir
  liftIO $ majorCommentBlock "DB"
  setupDBDir dbConfig
  liftIO $ majorCommentBlock "BACK-END"
  setupDir dbConfig backendDirConfig
  liftIO $ majorCommentBlock "FRONT-END"
  setupDir dbConfig frontendDirConfig

setupAllSubDirectories :: DBConfig -> ScriptRunContext ()
setupAllSubDirectories dbConfig = do
  setupOpsDir
  setupCoreDirectories dbConfig


setupDir :: DBConfig -> DirSetup -> ScriptRunContext ()
setupDir dbConfig dirSetup = do
  appRootDir' <- getAppRootDir
  let (dname, dPath) = getDir appRootDir' dirSetup
  getTemplate dPath dirSetup

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


setupDBDir :: DBConfig -> ScriptRunContext ()
setupDBDir dbConfig = do
  liftIO $ majorCommentBlock "SETTING UP DB"
  rootDir <- getAppRootDir
  fromAppRootDir
  mkdir "db"
  opsDir' <- getOpsDir
  let simpleMigrationDir = opsDir' </> fromText "db" </> fromText "migrations" </> fromText "haskell" </> fromText "pg-simple"
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
