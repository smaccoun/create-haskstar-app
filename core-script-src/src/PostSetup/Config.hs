{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module PostSetup.Config where

import           Context
import           Control.Lens    (over, set, (.~), (^.))
import           Control.Lens.TH
import           Data.Aeson
import           Data.Maybe      (fromMaybe)
import           Data.Text       (Text, pack)
import qualified Data.Text       as T (replace)
import qualified Data.Yaml       as YAML
import           DBConfig
import           GHC.Generics
import           Interactive     (prompt)
import           PostSetup.K8    (configureKubeSecrets)
import           Turtle

data HASMFile =
    HASMFile
      {_appName :: Text
      ,_remote  :: Maybe RemoteConfig
      } deriving (Generic)

data RemoteConfig =
    RemoteConfig
      {_dockerBaseImage :: Text
      ,_domain          :: Text
      ,_email           :: Text
      ,_dbRemoteConfig  :: Maybe RemoteDBConfig
      } deriving (Generic)

data RemoteDBConfig =
  RemoteDBConfig
    {_dbRemoteUser     :: Text
    ,_dbRemoteHost     :: Text
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
  case hasmFile' ^. remote of
    Just remoteConfig -> return remoteConfig
    Nothing -> die "You do not have a remote config setup in your HASM file"


interactiveConfigureDB :: Environment -> ScriptRunContext ()
interactiveConfigureDB env = do
  dbRemoteHostAnswer <- prompt "DB_HOST" Nothing
  dbRemoteUserAnswer <- prompt "DB_USER" (Just "postgres")
  dbRemotePasswordAnswer <- prompt "DB_PASSWORD" Nothing
  let baseRemoteDBConfig =
        RemoteDBConfig
          {_dbRemoteUser =  dbRemoteUserAnswer
          , _dbRemoteHost     = dbRemoteHostAnswer
          ,_dbRemotePassword = dbRemotePasswordAnswer
          }
  curHASMFile <- readHASMFile
  appName' <- getAppName
  dbConfig <- readDBConfig $ RemoteEnv Production --TODO: Make env specific
  case curHASMFile ^. remote of
    Just curRemoteConfig -> do
      let newRemoteConfig = set dbRemoteConfig (Just baseRemoteDBConfig) curRemoteConfig
          newHASMFile = set remote (Just newRemoteConfig) curHASMFile
      writeHASMFile newHASMFile
      _ <- configureKubeSecrets appName' dbConfig
      return ()
    Nothing ->
      die "You do not have a general remote config setup yet!"
    where
      writeHASMFile :: HASMFile -> ScriptRunContext ()
      writeHASMFile hasmFile = do
          hasmFilePath <- getHASMFilePathStr
          liftIO $ YAML.encodeFile hasmFilePath hasmFile
          return ()



readDBConfig :: Environment -> ScriptRunContext DBConfig
readDBConfig curEnv =
  case curEnv of
    Local ->
      mkDefaultLocalDBConfig <$> getAppName
    RemoteEnv rv -> do
      hasmFile' <- readHASMFile
      remoteConfig <- readRemoteConfig
      let remoteDBConfig' = remoteConfig ^. dbRemoteConfig
      appName' <- getAppName
      case remoteDBConfig' of
        Just rdb -> return $ defaultRemoteDBConfig appName' rdb
        Nothing ->
          die "You do not have config setup for a remote db. Please edit your HASMFIle to include remote DB Config"

defaultRemoteDBConfig :: Text -> RemoteDBConfig -> DBConfig
defaultRemoteDBConfig appName' dbConfig =
      DBConfig
        {dbHost = (dbConfig ^. dbRemoteHost)
        ,dbPort = 5432
        ,dbUser = dbConfig ^. dbRemoteUser
        ,dbPassword = dbConfig ^. dbRemotePassword
        ,dbName = T.replace "-" "_" appName'
        ,dbSchema = "public"
        }



deriveRemoteBaseImageName :: StackLayer -> ScriptRunContext Text
deriveRemoteBaseImageName stackLayer = do
  remoteConfig' <- readRemoteConfig
  let baseImage = remoteConfig' ^. dockerBaseImage
  return $ baseImage <> "-" <> (getSuffix stackLayer)
  where
    getSuffix stackLayer =
      case stackLayer of
        Backend  -> "backend"
        Frontend -> "frontend"

