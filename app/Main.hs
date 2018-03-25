#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Turtle
import Filesystem.Path.CurrentOS (fromText, encodeString)

parser :: Parser (Text, Maybe Text)
parser = (,) <$> argText "app-name" "Name of directory to put your app in"
             <*> optional (optText "front-end"  'a' "Choise of front-end")

main :: IO ()
main = do
  (appNameOption, mbfrontEndOption) <- options "Options" parser
  curDir <- pwd
  let appPath = curDir </> fromText (appNameOption)
  mkdir appPath
  let runOps = curDir </> "ops"
  putStrLn $ encodeString runOps
  chmod executable (runOps </> "ttab")
  cptree runOps appPath
  cd appPath
  setupDir appPath backendDirConfig
  setupDir appPath frontendDirConfig
  askToRun $ runServers appPath
  cd appPath
  return ()
