{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module PostSetup.K8 where

import qualified Configuration.Dotenv as Dotenv
import           Context
import           Data.Text            (Text, intercalate, pack)
import           Interactive          (showCommand)
import           PostSetup.Config     (getAppName)
import           Turtle

configureKubeSecrets :: ScriptRunContext [(String, String)]
configureKubeSecrets = do
  backendDir' <- getBackendDir
  appName' <- getAppName
  let backendEnvFile = backendDir' </> ".env"
  envVars <- Dotenv.parseFile $ encodeString backendEnvFile
  let fullCmd = (setKubeSecretsCmd appName' envVars)
  showCommand fullCmd
  _ <- shell fullCmd  empty
  return envVars

setKubeSecretsCmd :: Text -> [(String, String)] -> Text
setKubeSecretsCmd appName' envVars =
    configMapBaseCmd appName' <> (kubeSecretLiterals envVars) <> " --dry-run -o json | kubectl apply -f -"

configMapBaseCmd :: Text -> Text
configMapBaseCmd appName' =
    "kubectl create configmap " <> appName' <> "-envvars   "

kubeSecretLiterals :: [(String, String)] -> Text
kubeSecretLiterals envVars =
    envVars
    & fmap (\(eCat, eValue) -> pack $ "--from-literal=" <> eCat <> "=\"" <> eValue <> "\"")
    & intercalate "  "
