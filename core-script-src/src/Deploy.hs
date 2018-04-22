#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Deploy where

import           Context
import           Data.Text                 (pack)
import           Distribution.System
import           Filesystem.Path.CurrentOS (encodeString)
import           Interactive
import           Lib
import           PostSetup.Config
import           Turtle


deploy :: DeployConfig -> ScriptRunContext ()
deploy deployConfig = do
  fromAppRootDir
  cd "back-end"
  remoteDockerDir <- getRemoteDockerBaseDir deployConfig
  _ <- shell (dockerBuildRelative remoteDockerDir) empty
  dockerPushRelative remoteDockerDir
  return ()
  where
    dockerBuildRelative dr = dockerBuildStrCmd dr "Dockerfile ."
    dockerPushRelative dr = do
      _ <- shell (dockerPushCmd dr) empty
      return ()


getRemoteDockerBaseDir :: DeployConfig -> ScriptRunContext Text
getRemoteDockerBaseDir (DeployConfig mbRemoteDockerBaseDir mbSHA1 ) = do
  case mbRemoteDockerBaseDir of
    Just rd -> return $ tagRemoteDir rd mbSHA1
    Nothing -> do
      hasmFile <- readHASMFile
      case remoteDockerContainer hasmFile of
        Just f -> return $ tagRemoteDir (RemoteDockerBaseDir f) mbSHA1
        Nothing ->
          die $ "You must supply a remote docker container in your HASM file or using --remoteDockerDir"

tagRemoteDir :: RemoteDockerBaseDir -> Maybe SHA1 -> Text
tagRemoteDir (RemoteDockerBaseDir d) maybeSha1 =
    case maybeSha1 of
      Just (SHA1 sha1) -> d <> ":" <> sha1
      Nothing          -> d

dockerBuildStrCmd :: Text -> Turtle.FilePath -> Text
dockerBuildStrCmd remoteDockerDir localDockerFile =
  format ("docker build --rm=false -t "%s%" -f "%s%"") remoteDockerDir (pack $ encodeString localDockerFile)

dockerPushCmd :: Text -> Text
dockerPushCmd remoteDockerDir =
  format ("docker push "%s%"") remoteDockerDir

