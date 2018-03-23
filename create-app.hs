#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle

data DirSetup =
  DirSetup
    {dirName :: Text
    ,gitDir :: Text
    }

frontEndDirSetup =
  DirSetup
	  {dirName = "front-end"
	  ,gitDir = "git@github.com:simonh1000/elm-webpack-starter.git"
	  }

backEndDirSetup =
  DirSetup
	  {dirName = "back-end"
	  ,gitDir = "git@github.com:smaccoun/haskstar-haskell.git"
	  }

asLocal dirName = "./" <> dirName

majorCommentBlock :: Text -> IO ()
majorCommentBlock msg = do
  putStrLn "******************"
  echo $ unsafeTextToLine msg 
  putStrLn "******************"

setupDir rootDir dirSetup = do
  let dname = dirName dirSetup
  let dPath = rootDir </> (fromText dname)
  majorCommentBlock $ "Setting up " <> dname
  mkdir dPath
  cd dPath
  setupResult<- shell ("git clone " <> gitDir dirSetup) empty
  case setupResult of
	ExitSuccess   -> return ()
	ExitFailure n -> die (" failed with exit code: " <> repr n)
  

main :: IO ()
main = do
  topDir <- pwd
  setupDir topDir backEndDirSetup
  setupDir topDir frontEndDirSetup 
  return ()

