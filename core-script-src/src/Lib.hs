#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Context
import qualified Control.Foldl             as Fold
import           Control.Monad.Reader      (runReaderT)
import           Data.Aeson
import           Data.List                 (isInfixOf, sort)
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as Set
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import qualified Data.Yaml                 as YAML
import           Filesystem.Path.CurrentOS (decodeString, encodeString,
                                            fromText)
import           Interactive
import           PostSetup.Config          (HASMFile (..), getHASMFilePathStr)
import           System.Environment        (getExecutablePath)
import           Turtle                    hiding (Generic)
---------------------------------------------------------------

newtype SHA1 = SHA1 Text

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



writeHASMFile :: HASMFile -> ScriptRunContext ()
writeHASMFile hasmFile = do
    hasmFilePath <- getHASMFilePathStr
    liftIO $ YAML.encodeFile hasmFilePath hasmFile
    return ()

getExecutablePath' :: IO ExecutablePath
getExecutablePath' =
    fmap (ExecutablePath . decodeString) getExecutablePath


getPostSetupContext :: IO Context
getPostSetupContext = do
    appRootDir <- pwd
    executablePath <- getExecutablePath'
    return $ Context appRootDir executablePath Nothing

validateAndRunPostSetupCmd :: Context -> ScriptRunContext () -> IO ()
validateAndRunPostSetupCmd context cmd = do
    isValidHASMDir <- checkValidHASMDir
    if isValidHASMDir then do
        io cmd context
    else
        ioError $ userError "You are not in a valid HASM directory"





