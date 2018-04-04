
#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module StackBuild where

import           Context
import           Data.Text                 (Text)
import           DBConfig
import           DirSetup
import           Filesystem.Path.CurrentOS (encodeString)
import           Interactive
import           Turtle

buildFrontEnd :: ScriptRunContext ()
buildFrontEnd = do
  topDir <- getAppRootDir
  liftIO $ subCommentBlock "Building front-end"
  let frontEndPath = getDir topDir frontendDirConfig & snd
  liftIO $ putStrLn $ encodeString frontEndPath
  cd frontEndPath
  _ <- shell "yarn install" empty
  _ <- shell "elm-package install --yes" empty
  return ()

buildBackEnd :: DBConfig -> ScriptRunContext ()
buildBackEnd dbConfig = do
  liftIO $ subCommentBlock "Building back-end"
  fromAppRootDir
  topDir <- getAppRootDir
  let backendDir = getDir topDir backendDirConfig & snd
  cd backendDir
  _ <- shell "stack build" empty
  liftIO $ majorCommentBlock "SETUP DB CONFIGURATION"
  _ <- liftIO $ mkBackendEnv dbConfig backendDir
  return ()

buildFrontAndBackend :: DBConfig -> ScriptRunContext ()
buildFrontAndBackend dbConfig = do
  buildFrontEnd
  buildBackEnd dbConfig
