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
import qualified Data.Text.Lazy            as TL
import qualified Data.Yaml                 as YAML
import           Filesystem.Path.CurrentOS (decodeString, encodeString,
                                            fromText)
import           Interactive
import           System.Environment        (getExecutablePath)
import           Text.Mustache
import           Turtle                    hiding (Generic)
---------------------------------------------------------------

newtype AppName = AppName Text
newtype RemoteDockerImage = RemoteDockerImage Text
newtype Email = Email Text
newtype Domain = Domain Text
newtype SHA1 = SHA1 Text

io ::  Context -> ScriptRunContext () -> IO ()
io context (ScriptRunContext action) =
    runReaderT action context

gitCloneShallow :: Text -> Maybe Text -> IO ExitCode
gitCloneShallow gitRepo mbTemplate = do
  _ <- subCommentBlock $ "Cloning: " <> cloneCmd
  shell cloneCmd empty
  where
     gitBaseCloneCmd = "git clone --depth 1 "
     withBranch = fmap (\t -> "-b " <> t <> "  ") mbTemplate & fromMaybe ""
     cloneCmd = gitBaseCloneCmd <> withBranch <> gitRepo


isHASMFileDirectChild :: Turtle.FilePath -> IO Bool
isHASMFileDirectChild aDir = do
    directChildren <- fold (ls aDir) Fold.list
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
    curDir <- pwd
    mbAppRootDir <- isInValidHASMDir curDir
    case mbAppRootDir of
        Just appRootDir' ->  do
            executablePath <- getExecutablePath'
            return $ Context appRootDir' executablePath Nothing
        Nothing ->
            die "You are not within a valid HASM directory"

isInValidHASMDir :: Turtle.FilePath -> IO (Maybe Turtle.FilePath)
isInValidHASMDir curDir = do
    dirHasValidHASMFile <- isHASMFileDirectChild curDir
    if dirHasValidHASMFile  then
        return $ Just curDir
    else
        if curDir == "/" then
            return Nothing
        else
            isInValidHASMDir $ parent curDir


{- Applies template values to mustache file and writes to new file in place, without mustache extension -}
configureMustacheTemplate :: Turtle.FilePath -> Data.Aeson.Value -> ScriptRunContext Turtle.FilePath
configureMustacheTemplate templatePath decoder = do
  let mustacheFilename = filename templatePath & encodeString & T.pack
  compiledTemplate <- compileMustacheFile $ encodeString templatePath
  let writeToFilename = T.replace ".mustache" "" mustacheFilename
      configuredDeploymentFilepath = directory templatePath </> fromText writeToFilename
  _ <- liftIO $ writeTextFile configuredDeploymentFilepath
              $ TL.toStrict
              $ renderMustache compiledTemplate decoder
  return configuredDeploymentFilepath



