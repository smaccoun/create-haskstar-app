{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PostSetup.K8 where

import qualified Configuration.Dotenv as Dotenv
import           Context
import           Data.List            (find)
import           Data.Text            (Text, intercalate, pack, replace)
import           DBConfig             (DBConfig (..))
import           Interactive          (showCommand)
import           Turtle

data K8Commands =
    SetImage StackLayer

configureKubeSecrets :: Text -> DBConfig -> ScriptRunContext [(String, String)]
configureKubeSecrets appName' dbConfig = do
  backendDir' <- getBackendDir
  let backendEnvFile = backendDir' </> ".env"
  envVars <- Dotenv.parseFile $ encodeString backendEnvFile
  case getAuthJWK envVars of
    Nothing -> die "Cannot find configured AUTH JWK"
    Just jwk -> do
        let configmapVars = getConfigmapVars (pack jwk) dbConfig
        let fullCmd = (kubeSecretsCmd appName' configmapVars)
        showCommand fullCmd
        _ <- shell fullCmd  empty
        return envVars
  where
    getAuthJWK :: [(String, String)] -> Maybe String
    getAuthJWK envVars =
        Data.List.find ((==) "AUTH_JWK" . fst) envVars
          & fmap snd

getConfigmapVars :: Text -> DBConfig -> [(Text, Text)]
getConfigmapVars jwk DBConfig{..} =
    [("DB_PORT", pack $ show dbPort)
    ,("DB_SCHEMA", dbSchema)
    ,("DB_USERNAME", dbUser)
    ,("DB_PASSWORD", dbPassword)
    ,("DB_HOST", dbHost)
    ,("DB_DATABASE", dbName)
    ,("AUTH_JWK", Data.Text.replace "\"" "\\\"" jwk)
    ]

kubeSecretsCmd :: Text -> [(Text, Text)] -> Text
kubeSecretsCmd appName' envVars =
    configMapBaseCmd appName' <> (kubeSecretLiterals envVars) <> " --dry-run -o json | kubectl apply -f -"

configMapBaseCmd :: Text -> Text
configMapBaseCmd appName' =
    "kubectl create configmap " <> appName' <> "-envvars   "

kubeSecretLiterals :: [(Text, Text)] -> Text
kubeSecretLiterals envVars =
    envVars
    & fmap (\(eCat, eValue) -> "--from-literal=" <> eCat <> "=\"" <> eValue <> "\"")
    & intercalate "  "
