#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

data DirSetup =
  DirSetup
    {dirName :: Text
    ,gitDir :: Text
    }

frontendDirConfig =
  DirSetup
	  {dirName = "front-end"
	  ,gitDir = "git@github.com:simonh1000/elm-webpack-starter.git"
	  }

backendDirConfig =
  DirSetup
	  {dirName = "back-end"
	  ,gitDir = "git@github.com:smaccoun/haskstar-haskell.git"
	  }

asLocal dirName = "./" <> dirName

majorCommentBlock :: Text -> IO ()
majorCommentBlock msg = do
  putStrLn "******************"
  echo $ unsafeTextToLine msg 
  putStrLn "******************\n\n"

getDir :: Turtle.FilePath -> DirSetup -> (Text, Turtle.FilePath)
getDir rootDir dirSetup =
  (dname, dPath)
  where
    dname = dirName dirSetup
    dPath = rootDir </> (fromText dname)

setupDir :: Turtle.FilePath -> DirSetup -> IO ()
setupDir rootDir dirSetup = do
  let (dname, dPath) = getDir rootDir dirSetup
  majorCommentBlock $ "Setting up " <> dname
  setupResult<- shell ("git clone " <> gitDir dirSetup <> " " <> dname) empty
  case setupResult of
    ExitSuccess   -> return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)

buildFrontEnd topDir = do
  majorCommentBlock "BUILDING FRONT END"
  cd $ getDir topDir frontendDirConfig & snd
  _ <- shell "yarn install" empty
  _ <- shell "elm-package install --yes" empty
  return ()

main :: IO ()
main = do
  topDir <- pwd
  setupDir topDir backendDirConfig
  setupDir topDir frontendDirConfig
  buildFrontEnd topDir
  cd topDir
  return ()

