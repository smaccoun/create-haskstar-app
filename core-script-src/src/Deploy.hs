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
deploy (DeployConfig (RemoteDockerBaseDir remoteDockerBaseDir) sha1) = do
  fromAppRootDir
  cd "back-end"
  _ <- shell dockerBuildRelative empty
  _ <- shell dockerPushRelative empty
  return ()
  where
    dockerBuildRelative = dockerBuildStrCmd remoteDockerBaseDir "Dockerfile ."
    dockerPushRelative = dockerPushCmd remoteDockerBaseDir (Just sha1)


dockerBuildStrCmd :: Text -> Turtle.FilePath -> Text
dockerBuildStrCmd remoteDockerDir localDockerFile =
  format ("docker build --rm=false -t "%s%" -f "%s%"") remoteDockerDir (pack $ encodeString localDockerFile)

dockerPushCmd :: Text -> Maybe SHA1 -> Text
dockerPushCmd remoteDockerDir maybeSha1 =
  format ("docker push "%s%"") remoteDir
  where
    remoteDir = remoteDockerDir <> ":" <> tag
    tag =
        case maybeSha1 of
          Just (SHA1 sha1) -> sha1
          Nothing          -> ""
