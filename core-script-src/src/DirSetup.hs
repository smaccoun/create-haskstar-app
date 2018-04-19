#!/usr/bin/env runhaskell

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module DirSetup where

import           Context
import qualified Data.Aeson                as A
import qualified Data.ByteString.Lazy      as LBS
import           Data.Text                 (Text, intercalate, pack)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8)
import           Data.Text.Lazy.Builder    (toLazyText)
import           DBConfig
import           Filesystem.Path.CurrentOS (encodeString)
import           Interactive
import           Lib
import           Run                       (runDB)
import           Servant.Auth.Server       (generateKey)
import           Turtle


setupOpsDir :: ScriptRunContext ()
setupOpsDir = do
  fromAppRootDir
  mbTemplate <- getMbTemplate
  liftIO $ do
    majorCommentBlock "Grabbing required templates"
    mktree "./ops/db"
    _ <- gitCloneShallow "git@github.com:smaccoun/create-haskstar-app.git" mbTemplate
    cptree "./create-haskstar-app/templates/ops" "./ops"
    cptree "./create-haskstar-app/templates/db" "./ops/db"
    mv "./create-haskstar-app/templates/ci/.circleci" "./"
    rmtree "create-haskstar-app"

data DirSetup =
  DirSetup
    {dirStackType :: DirStackType
    ,dirName      :: Text
    ,gitDir       :: Text
    }

data DirStackType = FRONT_END | BACK_END

elmConfig :: DirSetup
elmConfig =
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
  mbTemplate <- getMbTemplate
  setupResult <- liftIO $ gitCloneShallow (gitDir dirSetup <> " " <> dname) mbTemplate
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
  majorCommentBlock "DB"
  setupDBDir dbConfig
  majorCommentBlock "BACK-END"
  setupDir dbConfig backendDirConfig
  majorCommentBlock "FRONT-END"
  setupDir dbConfig elmConfig

setupAllSubDirectories :: DBConfig -> ScriptRunContext ()
setupAllSubDirectories dbConfig = do
  setupOpsDir
  setupCoreDirectories dbConfig


setupDir :: DBConfig -> DirSetup -> ScriptRunContext ()
setupDir dbConfig dirSetup = do
  appRootDir' <- getAppRootDir
  let (dname, dPath) = getDir appRootDir' dirSetup
  getTemplate dPath dirSetup
  case dirStackType dirSetup of
    BACK_END  -> liftIO $ mkBackendEnv dbConfig dPath
    FRONT_END -> return ()


mkBackendEnv :: DBConfig -> Turtle.FilePath -> IO ()
mkBackendEnv (DBConfig host port dbName dbUser dbPassword dbSchema) backendDir = do
  jwkKey <- generateKey
  let textFile = T.intercalate "\n" $
         [ dbHostLn
         , T.pack $ dbPortLn port
         , dbDatabaseLn dbName
         , dbSchemaLn dbSchema
         , dbUserLn dbUser
         , dbPasswordLn dbPassword
         , jwkLine jwkKey
         ]
  writeTextFile (backendDir </> ".env") textFile

  where
    dbHostLn    = "DB_HOST=localhost"
    dbPortLn dbPort = "DB_PORT=" <> show dbPort
    dbDatabaseLn dbName   = "DB_DATABASE=" <> dbName
    dbSchemaLn schema     = "DB_SCHEMA=" <> schema
    dbUserLn dbUser = "DB_USERNAME=" <> dbUser
    dbPasswordLn password = "DB_PASSWORD=" <> dbPassword
    jwkLine jwkKey = "AUTH_JWK=" <> (T.replace "\"" "\\\"" . decodeUtf8 . LBS.toStrict . A.encode $ jwkKey)


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
  let dbEnvFile = textForDBEnvFile dbConfig rootDir
  liftIO $ writeTextFile ".env" dbEnvFile
  runDB


