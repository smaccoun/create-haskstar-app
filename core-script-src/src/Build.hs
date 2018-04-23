
#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Build where

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
  frontEndPath <- getFrontendDir
  liftIO $ putStrLn $ encodeString frontEndPath
  cd frontEndPath
  _ <- shell "yarn build" empty
  return ()

buildBackEnd :: ScriptRunContext ()
buildBackEnd = do
  liftIO $ subCommentBlock "Building back-end"
  fromAppRootDir
  topDir <- getAppRootDir
  backendDir <- getBackendDir
  cd backendDir
  _ <- shell "stack build" empty
  return ()

buildFrontAndBackEnd :: ScriptRunContext ()
buildFrontAndBackEnd = do
  buildFrontEnd
  buildBackEnd
