#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Deploy where

import           Context
import           Control.Lens
import           Data.Text                 (pack)
import           Filesystem.Path.CurrentOS (encodeString)
import           Interactive
import           Lib
import           PostSetup.Config
import           Run                       (runMigrations)
import           Turtle

deploy :: DeployConfig -> ScriptRunContext ()
deploy deployConfig = do
  deployBackend deployConfig
  runMigrations $ RemoteEnv Production
  return ()


deployBackend :: DeployConfig -> ScriptRunContext ExitCode
deployBackend deployConfig = do
  remoteDockerImage <- getRemoteDockerImage deployConfig
  k8 $ SetImage remoteDockerImage

getRemoteDockerImage :: DeployConfig -> ScriptRunContext Text
getRemoteDockerImage deployConfig = do
  shaToDeploy <- getShaToDeploy $ sha1 deployConfig
  case remoteDockerBaseDir deployConfig of
    Nothing -> do
      hasmFile' <- readHASMFile
      case hasmFile' ^. (remote . dockerBaseImage) of
        Just baseImage -> return $ baseImage <> shaToDeploy
        Nothing        -> die "You must supply a remote docker base image"
    Just (RemoteDockerBaseDir dockBaseImage) ->
      return $ dockBaseImage <> ":" <> shaToDeploy

getShaToDeploy :: Maybe SHA1 -> ScriptRunContext Text
getShaToDeploy mbSha1 =
  case mbSha1 of
    Nothing ->
      pack . show <$> single (inshell "git rev-parse HEAD" empty)
    Just (SHA1 s) ->
      return s

data K8Commands = SetImage Text

k8 :: K8Commands -> ScriptRunContext ExitCode
k8 kubeAction = do
  fromAppRootDir
  kubeStrCmd <- getKubeAction
  liftIO $ shell ("kubectl " <> kubeStrCmd) empty
  where
    getKubeAction =
      case kubeAction of
        SetImage image -> do
          appName' <- getAppName
          return $ " set image deployment/" <> appName' <> "   " <> image



getRemoteDockerBaseDir :: DeployConfig -> ScriptRunContext Text
getRemoteDockerBaseDir (DeployConfig mbRemoteDockerBaseDir mbSHA1 ) =
  case mbRemoteDockerBaseDir of
    Just rd -> return $ tagRemoteDir rd mbSHA1
    Nothing -> do
      hasmFile <- readHASMFile
      let remoteDockerImage = hasmFile ^. (remote . dockerBaseImage)
      case remoteDockerImage of
        Just f -> return $ tagRemoteDir (RemoteDockerBaseDir f) mbSHA1
        Nothing ->
          die "You must supply a remote docker container in your HASM file or using --remoteDockerDir"

tagRemoteDir :: RemoteDockerBaseDir -> Maybe SHA1 -> Text
tagRemoteDir (RemoteDockerBaseDir d) maybeSha1 =
    case maybeSha1 of
      Just (SHA1 sha1) -> d <> ":" <> sha1
      Nothing          -> d

dockerBuildStrCmd :: Text -> Turtle.FilePath -> Text
dockerBuildStrCmd remoteDockerDir localDockerFile =
  format ("docker build --rm=false -t "%s%" -f "%s%"") remoteDockerDir (pack $ encodeString localDockerFile)

dockerPushCmd :: Text -> Text
dockerPushCmd =
  format ("docker push "%s%"")

