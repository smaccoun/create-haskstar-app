#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Control.Foldl             as Fold
import           Control.Monad.Reader      (runReaderT)
import           Data.List                 (isInfixOf, sort)
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Filesystem.Path.CurrentOS (decodeString, encodeString,
                                            fromText)
import           System.Environment        (getExecutablePath)
import           Turtle

import           Context
import           Interactive
---------------------------------------------------------------

io :: ScriptRunContext () -> Context -> IO ()
io (ScriptRunContext action) context =
    runReaderT action context

gitCloneShallow :: Text -> Maybe Text -> IO ExitCode
gitCloneShallow gitRepo mbTemplate = do
  _ <- subCommentBlock $ "Cloning: " <> cloneCmd
  shell cloneCmd empty
  where
     gitBaseCloneCmd = "git clone --depth 1 "
     withBranch = fmap (\t -> "-b " <> t <> "  ") mbTemplate & fromMaybe ""
     cloneCmd = gitBaseCloneCmd <> withBranch <> gitRepo


checkValidHASMDir :: IO Bool
checkValidHASMDir = do
    directChildren <- fold (ls ".") Fold.list
    let directChildrenNames = fmap (encodeString . filename) directChildren
    return $ hasAllDirs directChildrenNames
    where
        requiredDirs = sort ["back-end", "front-end", "db"]
        hasAllDirs allChildrenNames =
            sort requiredDirs `isInfixOf` sort allChildrenNames


getExecutablePath' :: IO ExecutablePath
getExecutablePath' =
    fmap (ExecutablePath . decodeString) getExecutablePath
