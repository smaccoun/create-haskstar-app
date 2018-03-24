#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle


main :: IO ()
main = do
  topDir <- pwd
  setupDir topDir backendDirConfig
  setupDir topDir frontendDirConfig
  buildBackEnd topDir
  buildFrontEnd topDir
  runFrontEnd topDir
  cd topDir
  return ()


data DirSetup =
  DirSetup
    {dirName :: Text
    ,gitDir :: Text
    }

frontendDirConfig :: DirSetup
frontendDirConfig =
  DirSetup
	  {dirName = "front-end"
	  ,gitDir = "git@github.com:smaccoun/haskstar-elm.git"
	  }

backendDirConfig :: DirSetup
backendDirConfig =
  DirSetup
	  {dirName = "back-end"
	  ,gitDir = "git@github.com:smaccoun/haskstar-haskell.git"
	  }

asLocal dirName = "./" <> dirName

majorCommentBlock :: Text -> IO ()
majorCommentBlock msg = do
  printf "\n\n***********************************************\n"
  echo $ unsafeTextToLine msg
  printf "***********************************************\n\n"


getDir :: Turtle.FilePath -> DirSetup -> (Text, Turtle.FilePath)
getDir rootDir dirSetup =
  (dname, dPath)
  where
    dname = dirName dirSetup
    dPath = rootDir </> (fromText dname)

setupDir :: Turtle.FilePath -> DirSetup -> IO ()
setupDir rootDir dirSetup = do
  let (dname, dPath) = getDir rootDir dirSetup
  existingDir <- testdir dPath
  if existingDir then do
    echo "Found existing directory! Will remove and replace!"
    rmtree dPath
  else
    echo "Detected valid initial state"
  majorCommentBlock $ "Setting up " <> dname
  setupResult <- shell ("git clone " <> gitDir dirSetup <> " " <> dname) empty
  case setupResult of
    ExitSuccess   -> return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)

buildFrontEnd topDir = do
  majorCommentBlock "BUILDING FRONT END"
  cd $ getDir topDir frontendDirConfig & snd
  _ <- shell "yarn install" empty
  _ <- shell "elm-package install --yes" empty
  return ()

buildBackEnd topDir = do
  majorCommentBlock "BUILDING BACK END"
  cd $ getDir topDir backendDirConfig & snd
  _ <- shell "stack build" empty
  return ()


runFrontEnd topDir = do
  majorCommentBlock "STARTING WEB SERVER"
  cd $ getDir topDir frontendDirConfig & snd
  s <- shell "yarn start & " empty
  case s of
    ExitSuccess   -> do
        printf "\nSuccessfully started server. Go to localhost:3000\n"
        return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)
  return ()



