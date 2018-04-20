#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Control.Foldl             as Fold
import           Control.Monad.Reader      (runReaderT)
import           Data.Aeson
import           Data.List                 (isInfixOf, sort)
import           Data.Maybe                (fromMaybe)
import qualified Data.Set                  as Set
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import qualified Data.Yaml                 as YAML
import           Distribution.System
import           Filesystem.Path.CurrentOS (decodeString, encodeString,
                                            fromText)
import           GHC.Generics              hiding (empty)
import           System.Environment        (getExecutablePath)
import           Turtle                    hiding (Generic)

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


data HASMFile =
    HASMFile
      {appName               :: Maybe Text
      ,remoteDockerContainer :: Maybe Text
      } deriving (Generic, ToJSON, FromJSON)

writeHASMFile :: HASMFile -> ScriptRunContext ()
writeHASMFile hasmFile = do
    hasmFilePath <- getHASMFilePathStr
    liftIO $ YAML.encodeFile hasmFilePath hasmFile
    return ()

getHASMFilePathStr :: ScriptRunContext String
getHASMFilePathStr = do
    appRoot <- getAppRootDir
    let hasmFile = appRoot </> "HASMFile"
    return $ encodeString hasmFile

readHASMFile :: ScriptRunContext HASMFile
readHASMFile = do
    hasmFile <- getHASMFilePathStr
    hasmFileResult <- liftIO $ YAML.decodeFileEither hasmFile
    case hasmFileResult of
        Right f -> return f
        Left e  -> die $ "Something went wrong with hasm file: " <> T.pack (show e)

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
