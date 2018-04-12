{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Context where

import           Control.Monad.Reader      (ReaderT, ask, MonadReader)
import           Distribution.System
import           Filesystem.Path.CurrentOS
import           GHC.Generics
import           Turtle

newtype ScriptRunContext a =
  ScriptRunContext {unScriptRunContext :: ReaderT Context IO a
  }
  deriving (
     Functor
     , Applicative
     , Monad
     , MonadReader Context
     , MonadIO
     )

data Env = Development | Production deriving (Generic)

newtype ExecutablePath = ExecutablePath { unExecutablePath :: Turtle.FilePath }

data Context =
  Context
    {env        :: Env
    ,appRootDir :: Turtle.FilePath
    ,exeRootDir :: ExecutablePath
    ,curOS      :: OS
    ,mbTemplate :: Maybe Text
    }

getAppRootDir :: ScriptRunContext Turtle.FilePath
getAppRootDir = do
  Context{appRootDir} <- ask
  return appRootDir

fromAppRootDir :: ScriptRunContext ()
fromAppRootDir = do
  appRootDir <- getAppRootDir
  cd appRootDir

getCurOS :: ScriptRunContext OS
getCurOS = do
  context <- ask
  return (curOS context)

getOpsDir :: ScriptRunContext Turtle.FilePath
getOpsDir = do
  Context{appRootDir} <- ask
  return $ appRootDir </> decodeString "ops"

getTTab :: ScriptRunContext Turtle.FilePath
getTTab = do
  opsDir' <- getOpsDir
  return $ opsDir' </> decodeString "ttab"

getMbTemplate :: ScriptRunContext (Maybe Text)
getMbTemplate = do
  Context{mbTemplate} <- ask
  return mbTemplate
