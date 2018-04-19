#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Control.Foldl             as Fold
import qualified Data.Set as Set
import           Control.Monad.Reader      (runReaderT)
import           Data.List                 (isInfixOf, sort)
import           Data.Maybe                (fromMaybe)
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Distribution.System
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
    let directChildrenNames = Set.fromList $ fmap (encodeString . filename) directChildren
    return $ hasAllDirs directChildrenNames
    where
        requiredDirs = Set.fromList ["back-end", "front-end", "db"]
        hasAllDirs allChildrenNames =
            Set.isSubsetOf requiredDirs allChildrenNames


getExecutablePath' :: IO ExecutablePath
getExecutablePath' =
    fmap (ExecutablePath . decodeString) getExecutablePath


getPostSetupContext :: IO Context
getPostSetupContext = do
    appRootDir <- pwd
    executablePath <- getExecutablePath'
    return $ Context appRootDir executablePath buildOS Nothing

validateAndRunPostSetupCmd :: Context -> ScriptRunContext () -> IO ()
validateAndRunPostSetupCmd context cmd = do
    isValidHASMDir <- checkValidHASMDir
    if isValidHASMDir then do
        io cmd context
    else
        ioError $ userError "You are not in a valid HASM directory"
