#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Options where

import           Context          (RemoteStage (..))
import           PostSetup.Config
import           PostSetup.Deploy
import           Turtle

data ExecutionContext =
      New Text
    | PostSetupMode PostSetupOption

data PostSetupOption =
    Build BuildCmd
  | Start StartCmd
  | Run RunCmd
  | Deploy DeployConfig RemoteStage
  | Login LoginCmd

data BuildCmd = BuildFrontEnd | BuildBackEnd | BuildAll
data StartCmd = StartAPI | StartWeb | StartDB
data RunCmd = RunMigrations

data LoginCmd = LoginDB

parseCmd :: Parser ExecutionContext
parseCmd =
      fmap New (subcommand "new" "Setup new App" $ argText "appName" "Choose a name for your app")
  <|> fmap (\a -> PostSetupMode (Start a))
        (subcommand "start" "Start services" parseStartCmd)
  <|> fmap (\a -> PostSetupMode (Run a))
        (subcommand "run" "Run services" parseRunCmd)
  <|> fmap (\a -> PostSetupMode (Build a))
        (subcommand "build" "Build services" parseBuildCmd)
  <|> fmap mapDeployParse
        (subcommand "deploy" "Deploy services" parseDeployCmd)
  <|> fmap (\a -> PostSetupMode (Login a))
        (subcommand "login" "Login services" parseLoginCmd)
  where
    mapDeployParse :: (RemoteStage, Maybe Text, Maybe Text) -> ExecutionContext
    mapDeployParse (depEnv, sha1, remoteDockerDir) =
      PostSetupMode $ Deploy (mkDeployConfig sha1 remoteDockerDir) depEnv
    mkDeployConfig :: Maybe Text -> Maybe Text -> DeployConfig
    mkDeployConfig sha1 remoteDockerDir =
      DeployConfig
        {remoteDockerBaseDir = fmap RemoteDockerBaseDir remoteDockerDir
        ,sha1                = fmap SHA1 sha1
        }

parseStartCmd :: Parser StartCmd
parseStartCmd =
  arg parseStartText "startCmd" "Choose either 'front-end', 'back-end', or 'db'"
  where
    parseStartText rt =
      case rt of
        "back-end"  -> Just StartAPI
        "front-end" -> Just StartWeb
        "db"        -> Just StartDB
        _           -> Nothing

parseRunCmd :: Parser RunCmd
parseRunCmd =
  arg parseStartText "runCmd" "Choose 'migrations'"
  where
    parseStartText rt =
      case rt of
        "migrations" -> Just RunMigrations
        _            -> Nothing

parseLoginCmd :: Parser LoginCmd
parseLoginCmd =
  arg parseLoginText "loginCmd" "Choose 'db'"
  where
    parseLoginText rt =
      case rt of
        "db" -> Just LoginDB
        _    -> Nothing


parseDeployCmd :: Parser (RemoteStage, Maybe Text, Maybe Text)
parseDeployCmd =
  (,,)
  <$> arg parseDeployEnvText "deployCmd" "Choose either 'staging' or 'production'"
  <*> optional (optText "SHA1"  'a' "SHA1 Commit Number")
  <*> optional (optText "remoteDockerDir"  'a' "Remote Docker Dir")
  where
    parseDeployEnvText rt =
      case rt of
        "deploy"  -> Just Staging
        "staging" -> Just Production
        _         -> Nothing

parseBuildCmd :: Parser BuildCmd
parseBuildCmd =
  arg parseStartText "buildCmd" "Choose either 'front-end', 'back-end', or 'migrations'"
  where
    parseStartText rt =
      case rt of
        "back-end"  -> Just BuildBackEnd
        "front-end" -> Just BuildFrontEnd
        "all"       -> Just BuildAll
        _           -> Nothing


parser :: Parser (ExecutionContext, Maybe Text, Maybe Text)
parser = (,,)
             <$> parseCmd
             <*> optional (optText "front-end"  'a' "Choice of front-end")
             <*> optional (optText "template"  'a' "Choice of template")
