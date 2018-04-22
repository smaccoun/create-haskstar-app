{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module PostSetup.Config where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics

data HASMFile =
    HASMFile
      {appName               :: Maybe Text
      ,remoteDockerContainer :: Maybe Text
      } deriving (Generic, ToJSON, FromJSON)

newtype SHA1 = SHA1 Text
newtype RemoteDockerBaseDir = RemoteDockerBaseDir Text

data DeployConfig =
  DeployConfig
    {remoteDockerBaseDir :: Maybe RemoteDockerBaseDir
    ,sha1                :: Maybe SHA1
    }
