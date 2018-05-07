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
import qualified Data.Text       as T (replace)
import qualified Data.Yaml       as YAML
import           DBConfig
import           GHC.Generics
import           Turtle

data HASMFile =
    HASMFile
      {_appName :: Text
      ,_remote  :: RemoteConfig
      } deriving (Generic)

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
makeLenses ''HASMFile
makeLenses ''RemoteConfig

instance ToJSON HASMFile where
  toJSON = genericToJSON defaultOptions {
              fieldLabelModifier = drop 1 }

instance FromJSON HASMFile where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = drop 1 }

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

readRemoteConfig :: ScriptRunContext RemoteConfig
readRemoteConfig = do
  hasmFile' <- readHASMFile
  return $ hasmFile' ^. remote


readDBConfig :: Environment -> ScriptRunContext DBConfig
readDBConfig curEnv =
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
              {dbHost = dbConfig ^. dbRemoteHost
              ,dbPort = 5432
              ,dbUser = "postgres"
              ,dbPassword = dbConfig ^. dbRemotePassword
              ,dbName = T.replace "-" "_" (hasmFile' ^. appName)
              ,dbSchema = "public"
              }
        Nothing ->
          die "You do not have config setup for a remote db. Please edit your HASMFIle to include remote DB Config"



deriveRemoteBaseImageName :: StackLayer -> ScriptRunContext Text
deriveRemoteBaseImageName stackLayer = do
  remoteConfig' <- readRemoteConfig
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

