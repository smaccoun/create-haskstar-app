#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module DirSetup where

import           Context
import           Control.Lens              ((^.))
import qualified Data.Aeson                as A
import qualified Data.ByteString.Lazy      as LBS
import           Data.Text                 (Text, intercalate, pack)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8)
import qualified Data.Text.Lazy            as TL
import           Data.Text.Lazy.Builder    (toLazyText)
import           DBConfig
import qualified Filesystem.Path           as FP
import           Filesystem.Path.CurrentOS (decodeString, encodeString)
import           GHC.Generics
import           Interactive
import           Lib
import           PostSetup.Config
import           Run                       (runDB)
import           Servant.Auth.Server       (generateKey)
import           Text.Mustache
import           Turtle


data FrontEndSetupConfig =
  FrontEndSetupConfig
    {frontEndLang         :: FrontEndLang
    ,frontEndBaseTemplate :: GitBaseTemplateUrl
    }

data FrontEndLang =
    Elm
  | GHCJS
    deriving (Generic, Show, A.FromJSON)

runSetup :: Text -> DBConfig -> ScriptRunContext ()
runSetup appName' dbConfig = do
  writeHASMFile $ mkHasmFile Nothing
  shouldSetupResult <- liftIO $ promptYesNo "Setup remote deployment as part of initial setup (note: you can also complete this step after setup) ? "
  onShouldSetup shouldSetupResult
  setupAllSubDirectories dbConfig
  where
    onShouldSetup shouldSetup =
      case shouldSetup of
        Yes -> do
          dockerHubRepo <- liftIO $ prompt "What is your docker hub name? " Nothing
          let hasmFile = mkHasmFile $ Just dockerHubRepo
          writeHASMFile hasmFile
        No -> writeHASMFile $ mkHasmFile Nothing
    mkHasmFile mbDockerHubRepo =
      HASMFile
        {_appName = appName'
        ,_remote =
            RemoteConfig
              {_dockerBaseImage =
                  fmap (\dhr -> dhr <> "/" <> appName') mbDockerHubRepo
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

setupAllSubDirectories :: DBConfig -> ScriptRunContext ()
setupAllSubDirectories dbConfig = do
  setupOpsDir
  setupCoreDirectories dbConfig

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
    jwkLine jwkKey = "AUTH_JWK=" <> (T.replace "\"" "\\\"" . decodeUtf8 . LBS.toStrict . A.encode $ jwkKey)


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



setupOpsDir :: ScriptRunContext ()
setupOpsDir = do
  setupOpsTree
  configureCircle

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

getKubernetesDir :: ScriptRunContext Turtle.FilePath
getKubernetesDir = do
  opsDir' <- getOpsDir
  return $ opsDir' </> "kubernetes"

configureDeploymentScripts :: SHA1 -> ScriptRunContext ()
configureDeploymentScripts (SHA1 sha1) = do
  configureBackendServiceFile
  configureFrontendServiceFile
  configureCircle
  return ()

configureCircle :: ScriptRunContext ()
configureCircle = do
  topRootDir <- fromAppRootDir
  opsDir' <- getOpsDir
  let configStache = "config.yml.template"
      ciPath = opsDir' </> "ci"
      circleTemplatePath = ciPath </> ".circleci" </> fromText configStache
  hasmFile <- readHASMFile
  let circleTemplate = input circleTemplatePath
      mbDockerRepo = hasmFile ^. remote ^. dockerBaseImage
      circleConfigPath = (topRootDir </> ".circleci" </> "config.yml")
  writeCircleFile circleConfigPath mbDockerRepo circleTemplate

  return ()

writeCircleFile :: Turtle.FilePath -> Maybe Text -> Shell Line -> ScriptRunContext ()
writeCircleFile circleConfigPath mbDockerRepo circleTemplate =
  case mbDockerRepo of
    Just dockerRepo -> do
      output circleConfigPath $
       sed (fmap (\_ -> dockerRepo <> "-backend") $ text "<<dockerRepoBackendImage>>") circleTemplate &
       sed (fmap (\_ -> dockerRepo <> "-frontend") $ text "<<dockerRepoFrontendImage>>")
      return ()
    Nothing ->
      return ()


configureDeploymentFile :: StackLayer -> SHA1 -> ScriptRunContext Turtle.FilePath
configureDeploymentFile stackLayer sha1 = do
  (mustacheFilename, decoder) <- getDeploymentTemplateConfig stackLayer sha1
  configureK8StacheFile mustacheFilename decoder


configureK8StacheFile :: Text -> A.Value -> ScriptRunContext Turtle.FilePath
configureK8StacheFile stacheFile decoder = do
  kubernetesDir <- getKubernetesDir
  let mustacheFile = kubernetesDir </> fromText stacheFile
      mustacheFilename = filename mustacheFile & encodeString & pack
  stacheTemplate <- compileMustacheFile $ encodeString mustacheFile
  let writeToFilename = T.replace ".mustache" "" mustacheFilename
      configuredDeploymentFilepath = kubernetesDir </> fromText writeToFilename
  _ <- liftIO $ writeTextFile configuredDeploymentFilepath
              $ TL.toStrict
              $ renderMustache stacheTemplate decoder
  return configuredDeploymentFilepath


getDeploymentTemplateConfig :: StackLayer -> SHA1 -> ScriptRunContext (Text, A.Value)
getDeploymentTemplateConfig stackLayer (SHA1 curSha) = do
  hasmFile' <- readHASMFile
  dockerBaseImage <- deriveRemoteBaseImageName stackLayer
  case stackLayer of
    Frontend ->
      return $
        ("frontend-deployment.yaml.mustache"
        ,A.object
          [ "appName" A..= (hasmFile' ^. appName)
          , "remoteDockerImage" A..= (dockerBaseImage <> ":" <> curSha)
          ]
        )
    Backend  ->
      return $
        ("backend-deployment.yaml.mustache"
        ,A.object
          [ "appName" A..= (hasmFile' ^. appName)
          , "remoteDockerImage" A..= (dockerBaseImage <> ":" <> curSha)
          ]
        )

configureBackendServiceFile :: ScriptRunContext Turtle.FilePath
configureBackendServiceFile = do
  hasmFile <- readHASMFile
  let decoder =
          A.object
            [ "appName" A..= (hasmFile ^. appName)
          ]
  configureK8StacheFile "backend-service.yaml.mustache" decoder

configureFrontendServiceFile :: ScriptRunContext Turtle.FilePath
configureFrontendServiceFile = do
  hasmFile <- readHASMFile
  let decoder =
          A.object
            [ "appName" A..= (hasmFile ^. appName)
          ]
  configureK8StacheFile "frontend-service.yaml.mustache" decoder



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
elmFrontEndSetupConfig :: FrontEndSetupConfig
elmFrontEndSetupConfig =
  FrontEndSetupConfig
    {frontEndLang = Elm
    ,frontEndBaseTemplate = GitBaseTemplateUrl "git@github.com:smaccoun/haskstar-elm.git"
    }
