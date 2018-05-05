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
  deployBackend
  deployFrontend
  runMigrations $ RemoteEnv Production
  return ()


deployBackend :: ScriptRunContext ExitCode
deployBackend = do
  k8 $ SetImage Backend

deployFrontend :: ScriptRunContext ExitCode
deployFrontend = do
  k8 $ SetImage Frontend

getShaToDeploy :: Maybe SHA1 -> ScriptRunContext Text
getShaToDeploy mbSha1 =
  case mbSha1 of
    Nothing ->
      Turtle.strict $ single (inshell "git rev-parse HEAD" empty)
    Just (SHA1 s) ->
      return s

data K8Commands = SetImage StackLayer

k8 :: K8Commands -> ScriptRunContext ExitCode
k8 kubeAction = do
  fromAppRootDir
  kubeStrCmd <- getKubeAction
  let command = "kubectl " <> kubeStrCmd
  printfln $ "Running \"" <> command <> "\""
  liftIO $ shell ("kubectl " <> kubeStrCmd) empty
  where
    getKubeAction =
      case kubeAction of
        SetImage stackLayer -> do
          deployment <- k8DeploymentName stackLayer
          appName' <- getAppName
          k8ContainerName <- getK8ContainerName stackLayer
          remoteBaseImage <- deriveRemoteBaseImageName stackLayer
          sha1 <- getShaToDeploy Nothing
          let remoteImage = remoteBaseImage <> ":" <> sha1
          return $ " set image " <> deployment <> " " <> k8ContainerName <> "=" <> remoteImage

getK8ContainerName :: StackLayer -> ScriptRunContext Text
getK8ContainerName stackLayer = do
  appName <- getAppName
  return $ appName <> "-" <> dep
  where
    dep =
      case stackLayer of
        Backend  -> "backend"
        Frontend -> "frontend"

k8DeploymentName :: StackLayer -> ScriptRunContext Text
k8DeploymentName stackLayer = do
  k8Container <- getK8ContainerName stackLayer
  return $ "deployment/" <> k8Container


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

