{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module PostSetup.Config where

import           Control.Lens.TH
import           Data.Aeson
import           Data.Text       (Text)
import           GHC.Generics

data HASMFile =
    HASMFile
      {_appName :: Maybe Text
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
      {_dockerImage   :: Maybe Text
      } deriving (Generic)

instance ToJSON RemoteConfig where
  toJSON = genericToJSON defaultOptions {
              fieldLabelModifier = drop 1 }

instance FromJSON RemoteConfig where
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
