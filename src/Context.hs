module Context where

import Turtle
import           Control.Monad.Reader (ReaderT, ask)

type App = ReaderT Context IO

data Context =
  Context
    {appRootDir :: Turtle.FilePath
    ,exeRootDir :: Turtle.FilePath
    }

getAppRootDir :: App Turtle.FilePath
getAppRootDir = do
  context <- ask
  return (appRootDir context)

fromAppRootDir :: App ()
fromAppRootDir = do
  context <- ask
  cd (appRootDir context)

