#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad.Reader (runReaderT)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Turtle
import Data.Maybe (fromMaybe)

import           Context
import           Interactive
---------------------------------------------------------------

io :: ScriptRunContext () -> Context -> IO ()
io action context =
    runReaderT action context

gitCloneShallow :: Text -> Maybe Text -> IO ExitCode
gitCloneShallow gitRepo mbTemplate = do
  _ <- subCommentBlock $ "Cloning: " <> cloneCmd
  shell cloneCmd empty
  where
     gitBaseCloneCmd = "git clone --depth 1 "
     withBranch = fmap (\t -> "-b " <> t <> "  ") mbTemplate & fromMaybe ""
     cloneCmd = gitBaseCloneCmd <> withBranch <> gitRepo
