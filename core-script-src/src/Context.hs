{-# LANGUAGE DeriveGeneric #-}

module Context where

import           Control.Monad.Reader      (ReaderT, ask)
import           Distribution.System
import           Filesystem.Path.CurrentOS
import           GHC.Generics
import           Turtle

type ScriptRunContext = ReaderT Context IO

data Env = Development | Production deriving (Generic)

newtype ExecutablePath = ExecutablePath { unExecutablePath :: Turtle.FilePath }

data Context =
  Context
    {env        :: Env
    ,appRootDir :: Turtle.FilePath
    ,exeRootDir :: ExecutablePath
    ,opsDir     :: Turtle.FilePath
    ,curOS      :: OS
    ,mbTemplate :: Maybe Text
    }

setContext :: Env -> Turtle.FilePath -> ExecutablePath -> OS -> Maybe Text -> Context
setContext runEnv appDir executablePath curOS mbTemplate =
  Context runEnv appDir executablePath opsDir' curOS mbTemplate
  where
    opsDir' =  appDir </> decodeString "ops"


getAppRootDir :: ScriptRunContext Turtle.FilePath
getAppRootDir = do
  context <- ask
  return (appRootDir context)

fromAppRootDir :: ScriptRunContext ()
fromAppRootDir = do
  context <- ask
  cd (appRootDir context)

getCurOS :: ScriptRunContext OS
getCurOS = do
  context <- ask
  return (curOS context)

getOpsDir :: ScriptRunContext Turtle.FilePath
getOpsDir = do
  config <- ask
  return $ opsDir config

getTTab :: ScriptRunContext Turtle.FilePath
getTTab = do
  opsDir' <- getOpsDir
  return $ opsDir' </> decodeString "ttab"
