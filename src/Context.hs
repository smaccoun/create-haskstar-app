module Context where

import           Control.Monad.Reader      (ReaderT, ask)
import           Distribution.System
import           Filesystem.Path.CurrentOS
import           Turtle

type ScriptRunContext = ReaderT Context IO

data Context =
  Context
    {appRootDir   :: Turtle.FilePath
    ,exeRootDir   :: Turtle.FilePath
    ,opsDir       :: Turtle.FilePath
    ,templatesDir :: Turtle.FilePath
    ,curOS        :: OS
    }


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

getTemplateDir :: ScriptRunContext Turtle.FilePath
getTemplateDir = do
  config <- ask
  return $ templatesDir config

getTTab :: ScriptRunContext Turtle.FilePath
getTTab = do
  opsDir' <- getOpsDir
  return $ opsDir' </> decodeString "ttab"
