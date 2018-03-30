module Context where

import           Control.Monad.Reader      (ReaderT, ask)
import           Distribution.System
import           Filesystem.Path.CurrentOS
import           Turtle

type App = ReaderT Context IO

data Context =
  Context
    {appRootDir :: Turtle.FilePath
    ,exeRootDir :: Turtle.FilePath
    ,opsDir     :: Turtle.FilePath
    ,curOS      :: OS
    }


getAppRootDir :: App Turtle.FilePath
getAppRootDir = do
  context <- ask
  return (appRootDir context)

fromAppRootDir :: App ()
fromAppRootDir = do
  context <- ask
  cd (appRootDir context)

getCurOS :: App OS
getCurOS = do
  context <- ask
  return (curOS context)

getOpsDir :: App Turtle.FilePath
getOpsDir = do
  config <- ask
  return $ opsDir config

getTTab :: App Turtle.FilePath
getTTab = do
  opsDir' <- getOpsDir
  return $ opsDir' </> decodeString "ttab"

