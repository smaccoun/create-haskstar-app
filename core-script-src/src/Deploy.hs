#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Deploy where

import           Context
import           Data.Text                 (pack)
import           Distribution.System
import           Filesystem.Path.CurrentOS (encodeString)
import           Interactive
import           Turtle

newtype SHA1 = SHA1 Text
newtype RemoteDockerBaseDir = RemoteDockerBaseDir Text

data DeployConfig =
  DeployConfig
    {remoteDockerBaseDir :: RemoteDockerBaseDir
    ,sha1                :: SHA1
    }

deploy :: DeployConfig -> ScriptRunContext ()
deploy (DeployConfig (RemoteDockerBaseDir remoteDockerBaseDir) (SHA1 sha1)) = do
  fromAppRootDir
  cd "back-end"
  _ <- shell dockerBuildRelative empty
  _ <- shell dockerPushRelative empty
  return ()
  where
    dockerBuildRelative = dockerBuildStrCmd remoteDockerDir "Dockerfile ."
    dockerPushRelative = dockerPushCmd remoteDockerDir
    remoteDockerDir = remoteDockerBaseDir <> sha1


dockerBuildStrCmd :: Text -> Turtle.FilePath -> Text
dockerBuildStrCmd remoteDockerDir localDockerFile =
  format ("docker build --rm=false -t "%s%" -f "%s%"") remoteDockerDir (pack $ encodeString localDockerFile)

dockerPushCmd :: Text -> Text
dockerPushCmd remoteDockerDir =
  format ("docker push "%s%"") remoteDockerDir
