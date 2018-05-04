{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module PostSetup.Config where

import           Context
import           Control.Lens    ((^.))
import           Control.Lens.TH
import           Data.Aeson
import           Data.Text       (Text, pack)
import qualified Data.Yaml       as YAML
import           DBConfig
import           GHC.Generics
import           Turtle

data HASMFile =
    HASMFile
      {_appName :: Text
      ,_remote  :: RemoteConfig
      } deriving (Generic)

instance ToJSON HASMFile where
  toJSON = genericToJSON defaultOptions {
              fieldLabelModifier = drop 1 }

instance FromJSON HASMFile where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

data RemoteConfig =
    RemoteConfig
      {_dockerBaseImage :: Maybe Text
      ,_dbRemoteConfig  :: Maybe RemoteDBConfig
      } deriving (Generic)

data RemoteDBConfig =
  RemoteDBConfig
    {_dbRemoteHost     :: Text
    ,_dbRemotePassword :: Text
    } deriving (Generic)

makeLenses ''RemoteDBConfig

instance ToJSON RemoteConfig where
  toJSON = genericToJSON defaultOptions {
              fieldLabelModifier = drop 1 }

instance FromJSON RemoteConfig where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

instance ToJSON RemoteDBConfig where
  toJSON = genericToJSON defaultOptions {
              fieldLabelModifier = drop 1 }

instance FromJSON RemoteDBConfig where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

makeLenses ''HASMFile
makeLenses ''RemoteConfig

newtype SHA1 = SHA1 Text
newtype RemoteDockerBaseDir = RemoteDockerBaseDir Text

data DeployConfig =
  DeployConfig
    {remoteDockerBaseDir :: Maybe RemoteDockerBaseDir
    ,sha1                :: Maybe SHA1
    }

makeClassy ''DeployConfig

getAppName :: ScriptRunContext Text
getAppName = do
    hasmFile <- readHASMFile
    return $ hasmFile ^. appName

getHASMFilePathStr :: ScriptRunContext String
getHASMFilePathStr = do
    appRoot <- getAppRootDir
    let hasmFile = appRoot </> "HASMFile"
    return $ encodeString hasmFile

readHASMFile :: ScriptRunContext HASMFile
readHASMFile = do
    hasmFile <- getHASMFilePathStr
    hasmFileResult <- liftIO $ YAML.decodeFileEither hasmFile
    case hasmFileResult of
        Right f -> return f
        Left e  -> die $ "Something went wrong with hasm file: " <> pack (show e)

getRemoteConfig :: ScriptRunContext RemoteConfig
getRemoteConfig = do
  hasmFile' <- readHASMFile
  return $ hasmFile' ^. remote


getDBConfig :: Environment -> ScriptRunContext DBConfig
getDBConfig curEnv =
  case curEnv of
    Local ->
      mkDefaultLocalDBConfig <$> getAppName
    RemoteEnv rv -> do
      hasmFile' <- readHASMFile
      let remoteDBConfig' = hasmFile' ^. remote ^. dbRemoteConfig
      case remoteDBConfig' of
        Just dbConfig ->
          return
            DBConfig
              {host = dbConfig ^. dbRemoteHost
              ,port = 5432
              ,dbUser = "postgres"
              ,dbPassword = dbConfig ^. dbRemotePassword
              ,dbName = hasmFile' ^. appName
              ,dbSchema = "public"
              }
        Nothing ->
          die "You do not have config setup for a remote db. Please edit your HASMFIle to include remote DB Config"

data StackLayer = Frontend | Backend

deriveRemoteBaseImageName :: StackLayer -> ScriptRunContext Text
deriveRemoteBaseImageName stackLayer = do
  remoteConfig' <- getRemoteConfig
  let mbBaseImage = remoteConfig' ^. dockerBaseImage
  case mbBaseImage of
    Just baseImage ->
      return $ baseImage <> "-" <> (getSuffix stackLayer)
    Nothing ->
      die "Must have a base remote docker image configured"
  where
    getSuffix stackLayer =
      case stackLayer of
        Backend  -> "backend"
        Frontend -> "frontend"

