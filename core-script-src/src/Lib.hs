#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad.Reader (runReaderT)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Turtle

import           Context
import           Interactive
---------------------------------------------------------------

io :: ScriptRunContext () -> Context -> IO ()
io action context =
    runReaderT action context

gitCloneShallow :: Text -> IO ExitCode
gitCloneShallow gitRepo =
  shell cloneCmd empty
  where
     cloneCmd = "git clone --depth 1 " <> gitRepo
