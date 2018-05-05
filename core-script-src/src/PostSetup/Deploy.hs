#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module PostSetup.Deploy where

import           Context
import           Control.Lens
import           Data.Text                 (pack)
import           DirSetup                  (configureDeploymentFile)
import           Filesystem.Path.CurrentOS (encodeString)
import           Interactive
import           Lib
import           PostSetup.Config
import           PostSetup.K8              (configureKubeSecrets)
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
  _ <- configureKubeSecrets
  k8 $ SetImage Backend

deployFrontend :: ScriptRunContext ExitCode
deployFrontend = do
  k8 $ SetImage Frontend

getShaToDeploy :: Maybe SHA1 -> ScriptRunContext SHA1
getShaToDeploy mbSha1 =
  case mbSha1 of
    Nothing -> do
      sha1Text <- Turtle.strict $ single (inshell "git rev-parse HEAD" empty)
      return $ SHA1 sha1Text
    Just sha1 ->
      return sha1

data K8Commands =
  SetImage StackLayer

k8 :: K8Commands -> ScriptRunContext ExitCode
k8 kubeAction = do
  fromAppRootDir
  kubeStrCmd <- getKubeAction kubeAction
  let command = "kubectl " <> kubeStrCmd
  printfln $ "Running \"" <> command <> "\""
  liftIO $ shell command empty


getKubeAction :: K8Commands -> ScriptRunContext Text
getKubeAction kubeAction =
  case kubeAction of
    SetImage stackLayer -> do
      sha1 <- getShaToDeploy Nothing
      deploymentFile <- configureDeploymentFile stackLayer sha1
      return $ " apply -f " <> (pack . encodeString $ deploymentFile)

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

