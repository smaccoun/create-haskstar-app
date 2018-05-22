#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module DirSetup where

import qualified Configuration.Dotenv      as Dotenv
import           Context
import           Control.Lens              ((^.))
import qualified Data.Aeson                as A
import qualified Data.ByteString.Lazy      as LBS
import           Data.Text                 (Text, intercalate, pack)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8)
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Builder    (toLazyText)
import qualified Data.Yaml                 as YAML
import           DBConfig
import qualified Filesystem.Path           as FP
import           Filesystem.Path.CurrentOS (decodeString, encodeString)
import           GHC.Generics
import           Interactive
import           Lib
import           PostSetup.Config
import           PostSetup.K8Templates
import           Run                       (runDB)
import           Servant.Auth.Server       (generateKey)
import           Text.Mustache
import           Turtle

runSetup :: AppName -> DBConfig -> ScriptRunContext ()
runSetup appName'@(AppName appNameText) dbConfig = do
  shouldSetupRemote <- liftIO $ promptYesNo "Setup remote deployment as part of initial setup (note: you can also complete this step after setup) ? "
  case shouldSetupRemote of
    Yes -> do
      dockerHubRepo <- liftIO $ prompt "What is your docker hub name? " Nothing
      domain <- liftIO $ prompt "What primary domain will this be associated with (e.g. example.com)? " Nothing
      email <- liftIO $ prompt "What is your email (used for TLS cert auto renewal)? " Nothing
      let hasmFile = mkBaseHasmFile appName' (Email email) (Domain domain) dockerHubRepo
      writeHASMFile hasmFile

    No -> do
      writeHASMFile $ HASMFile appNameText Nothing

  setupOpsDir shouldSetupRemote
  setupCoreDirectories dbConfig


mkBaseHasmFile :: AppName -> Email -> Domain -> Text -> HASMFile
mkBaseHasmFile (AppName appName') (Email email') (Domain domain) dockerHubRepo =
  HASMFile
    {_appName = appName'
    ,_remote = Just $
        RemoteConfig
          {_dockerBaseImage =
              dockerHubRepo <> "/" <> appName'
          ,_domain = domain
          ,_email = email'
          ,_dbRemoteConfig = Nothing
          }
    }

-- | Setup DB, Front-End, Back-End directories without building them
setupCoreDirectories :: DBConfig -> ScriptRunContext ()
setupCoreDirectories dbConfig = do
  appDir <- getAppRootDir
  majorCommentBlock "DB"
  setupDBDir dbConfig
  majorCommentBlock "BACK-END"
  setupBackendDir dbConfig
  majorCommentBlock "FRONT-END"
  setupFrontendDir elmFrontEndSetupConfig


setupBackendDir :: DBConfig -> ScriptRunContext ()
setupBackendDir dbConfig = do
  cloneTemplateAsDir backendBaseGitUrl (CoreStackDirName "back-end")
  mkBackendEnv dbConfig
  return ()
  where
    backendBaseGitUrl = GitBaseTemplateUrl "https://github.com/smaccoun/haskstar-haskell"

setupFrontendDir :: FrontEndSetupConfig -> ScriptRunContext ()
setupFrontendDir frc@(FrontEndSetupConfig _ gitBaseUrl ) = do
  cloneTemplateAsDir gitBaseUrl (CoreStackDirName "front-end")
  buildFrontendBaseLibraries frc
  return ()

buildFrontendBaseLibraries :: FrontEndSetupConfig -> ScriptRunContext ()
buildFrontendBaseLibraries _ = do
  --TODO: Handle GHCJS initial compile case
  frontendDir <- getFrontendDir
  cd frontendDir
  _ <- shell "yarn install" empty
  _ <- shell "elm-package install --yes" empty
  return ()

mkBackendEnv :: DBConfig -> ScriptRunContext ()
mkBackendEnv (DBConfig host port dbName dbUser dbPassword dbSchema) = do
  jwkKey <- liftIO generateKey
  let textFile = T.intercalate "\n" $
         [ dbHostLn
         , T.pack $ dbPortLn port
         , dbDatabaseLn dbName
         , dbSchemaLn dbSchema
         , dbUserLn dbUser
         , dbPasswordLn dbPassword
         , jwkLine jwkKey
         ]
  backendDir <- getBackendDir
  liftIO $ writeTextFile (backendDir </> ".env") textFile
  return ()

  where
    dbHostLn    = "DB_HOST=localhost"
    dbPortLn dbPort = "DB_PORT=" <> show dbPort
    dbDatabaseLn dbName   = "DB_DATABASE=" <> dbName
    dbSchemaLn schema     = "DB_SCHEMA=" <> schema
    dbUserLn dbUser = "DB_USERNAME=" <> dbUser
    dbPasswordLn password = "DB_PASSWORD=" <> dbPassword
    jwkLine jwkKey = "AUTH_JWK=" <> writeJWKFromEnv jwkKey

writeJWKFromEnv jwkKey =
  (T.replace "\"" "\\\"" . decodeUtf8 . LBS.toStrict . A.encode $ jwkKey)


setupDBDir :: DBConfig -> ScriptRunContext ()
setupDBDir dbConfig = do
  liftIO $ majorCommentBlock "SETTING UP DB"
  dbDir <- getDBDir
  opsDir' <- getOpsDir
  let templateMigrationPackage = (fmap decodeString) ["db", "migrations", "haskell",  "pg-simple"] & FP.concat
      simpleMigrationDir = opsDir' </> templateMigrationPackage
  cptree simpleMigrationDir dbDir
  cd dbDir
  let dbEnvFile = textForDBEnvFile dbConfig
  liftIO $ writeTextFile ".env" dbEnvFile

  _ <- shell "stack build" empty
  runDB



setupOpsDir :: YesNo -> ScriptRunContext ()
setupOpsDir shouldSetupRemote = do
  setupOpsTree
  case shouldSetupRemote of
    Yes ->  do
      configureCircle
      configureInitialK8Templates
    No -> return ()

configureInitialK8Templates :: ScriptRunContext ()
configureInitialK8Templates = do
  hasmFile' <- readHASMFile
  let appName' = hasmFile' ^. appName
  case hasmFile' ^. remote of
    Just rc -> do
      let email' = rc ^. email
      let domain' = rc ^. domain
      _ <- configureK8StacheFile $ certIssuerConfig (Email email')
      _ <- setupGlobalGoogleStaticIPCmd
      _ <- configureK8StacheFile $ ingressConfig (AppName appName') (Domain domain')
      return ()
    Nothing ->
      return ()

setupGlobalGoogleStaticIPCmd :: ScriptRunContext (ExitCode)
setupGlobalGoogleStaticIPCmd = do
  appName' <- getAppName
  let cmd = "gcloud compute addresses create " <> appName' <> "-ip --global"
  r <- shell cmd empty
  return r


setupOpsTree :: ScriptRunContext ()
setupOpsTree = do
    fromAppRootDir
    topDir <- getAppRootDir
    mbTemplate <- getMbTemplate
    kubernetesDir <- getKubernetesDir
    liftIO $ do
      majorCommentBlock "Grabbing required templates"
      mktree "./ops/db"
      _ <- gitCloneShallow "git@github.com:smaccoun/create-haskstar-app.git" mbTemplate
      cptree "./create-haskstar-app/templates/ops" "./ops"
      cptree "./create-haskstar-app/templates/db" "./ops/db"
      cptree "./create-haskstar-app/templates/kubernetes" kubernetesDir
      let circleDir = topDir </> ".circleci"
      mkdir circleDir
      cptree "./ops/ci/.circleci/" circleDir
      rmtree "create-haskstar-app"

configureDeploymentScripts :: SHA1 -> ScriptRunContext ()
configureDeploymentScripts (SHA1 sha1) = do
  configureCircle
  return ()

configureCircle :: ScriptRunContext ()
configureCircle = do
  topRootDir <- fromAppRootDir
  opsDir' <- getOpsDir
  let configStache = "config.yml.template"
      ciPath = opsDir' </> "ci"
      circleTemplatePath = ciPath </> ".circleci" </> fromText configStache
  remoteConfig <- readRemoteConfig
  let circleTemplate = input circleTemplatePath
      dockerRepo = remoteConfig ^. dockerBaseImage
      circleConfigPath = (topRootDir </> ".circleci" </> "config.yml")
  writeCircleFile circleConfigPath dockerRepo circleTemplate

  return ()

writeCircleFile :: Turtle.FilePath -> Text -> Shell Line -> ScriptRunContext ()
writeCircleFile circleConfigPath dockerRepo circleTemplate = do
  output circleConfigPath $
    sed (fmap (\_ -> dockerRepo <> "-backend") $ text "<<dockerRepoBackendImage>>") circleTemplate &
    sed (fmap (\_ -> dockerRepo <> "-frontend") $ text "<<dockerRepoFrontendImage>>")
  return ()

writeHASMFile :: HASMFile -> ScriptRunContext ()
writeHASMFile hasmFile = do
    hasmFilePath <- getHASMFilePathStr
    liftIO $ YAML.encodeFile hasmFilePath hasmFile
    return ()


configureDeploymentFile :: StackLayer -> SHA1 -> ScriptRunContext Turtle.FilePath
configureDeploymentFile stackLayer sha1 = do
  stacheTemplate <- getDeploymentTemplateConfig stackLayer sha1
  configureK8StacheFile stacheTemplate


configureK8StacheFile :: StacheTemplate -> ScriptRunContext Turtle.FilePath
configureK8StacheFile (StacheTemplate stacheFilename configObj) = do
  kubernetesDir <- getKubernetesDir
  let templatePath = kubernetesDir </> fromText stacheFilename
  configureMustacheTemplate templatePath configObj


getDeploymentTemplateConfig :: StackLayer -> SHA1 -> ScriptRunContext StacheTemplate
getDeploymentTemplateConfig stackLayer (SHA1 curSha) = do
  hasmFile' <- readHASMFile
  dockerBaseImage <- deriveRemoteBaseImageName stackLayer
  let appName' = hasmFile' ^. appName
      dockerImage' = (dockerBaseImage <> ":" <> curSha)
  case stackLayer of
    Frontend ->
      return $ frontendDeploymentConfig (AppName appName')  (RemoteDockerImage dockerImage')
    Backend  ->
      return $ backendDeploymentConfig (AppName appName')  (RemoteDockerImage dockerImage')

newtype GitBaseTemplateUrl = GitBaseTemplateUrl Text
newtype CoreStackDirName =  CoreStackDirName Text

cloneTemplateAsDir :: GitBaseTemplateUrl -> CoreStackDirName -> ScriptRunContext ()
cloneTemplateAsDir (GitBaseTemplateUrl gitUrl) (CoreStackDirName dirName) = do
  liftIO $ subCommentBlock $ "Setting up " <> dirName
  appRootDir <- fromAppRootDir
  mbTemplate <- getMbTemplate
  setupResult <- liftIO $ gitCloneShallow (gitUrl <> " " <> dirName) mbTemplate
  case setupResult of
    ExitSuccess   -> do
      liftIO $ rmtree (fromText dirName </> fromText ".git")
      return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)


--TODO: Read this in from config file
data FrontEndSetupConfig =
  FrontEndSetupConfig
    {frontEndLang         :: FrontEndLang
    ,frontEndBaseTemplate :: GitBaseTemplateUrl
    }

data FrontEndLang =
    Elm
  | GHCJS
    deriving (Generic, Show, A.FromJSON)

elmFrontEndSetupConfig :: FrontEndSetupConfig
elmFrontEndSetupConfig =
  FrontEndSetupConfig
    {frontEndLang = Elm
    ,frontEndBaseTemplate = GitBaseTemplateUrl "git@github.com:smaccoun/haskstar-elm.git"
    }
