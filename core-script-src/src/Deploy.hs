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

deploy :: SHA1 -> RemoteDockerBaseDir -> ScriptRunContext ()
deploy (SHA1 sha1) (RemoteDockerBaseDir remoteDockerBaseDir) =
  fromAppRootDir
  cd "back-end"
  _ <- shell dockerBuildRelative empty
  _ <- shell dockerPushRelative empty
  where
    dockerBuildRelative = dockerBuildStrCmd remoteDockerDir "Dockerfile ."
    dockerPushRelative = dockerPushCmd remoteDockerDir
    remoteDockerDir = remoteDockerBaseDir <> sha1


dockerBuildStrCmd :: Text -> Turtle.FilePath -> Text
dockerBuildStrCmd remoteDockerDir localDockerFile =
  format ("docker build --rm=false -t "%s%" -f "%s%"") remoteDockerDir localDockerFile

dockerPushCmd :: Text -> Text
dockerPushCmd remoteDockerDir =
  format ("docker push "%s%"") remoteDockerDir
