#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Turtle
import Filesystem.Path.CurrentOS (encodeString)
import qualified Data.Text as T

askToRun onYes = do
  echo "Setup Complete! Would you like to boot up the servers? (y) yes, (n) no"
  maybeAnswer <- readline
  case maybeAnswer of
     Just a ->
       case a of
         "y" -> onYes
         "n" -> echo "You can boot up each server by running ./run.sh"
         _ -> echo "Please entery (y) or (n)"
     Nothing ->
        echo "Please entery (y) or (n)"

runServers topDir = do
    runBackEnd topDir
    runFrontEnd topDir


data DirSetup =
  DirSetup
    {dirStackType :: DirStackType
    ,dirName :: Text
    ,gitDir :: Text
    }

data DirStackType = FRONT_END | BACK_END

frontendDirConfig :: DirSetup
frontendDirConfig =
  DirSetup
	  {dirStackType = FRONT_END
	  ,dirName = "front-end"
	  ,gitDir = "git@github.com:smaccoun/haskstar-elm.git"
	  }

backendDirConfig :: DirSetup
backendDirConfig =
  DirSetup
	  {dirStackType = BACK_END
	  ,dirName = "back-end"
	  ,gitDir = "git@github.com:smaccoun/haskstar-haskell.git"
	  }

asLocal dirName = "./" <> dirName

majorCommentBlock :: Text -> IO ()
majorCommentBlock msg = do
  printf "\n\n***********************************************\n"
  echo $ unsafeTextToLine msg
  printf "***********************************************\n\n"


getDir :: Turtle.FilePath -> DirSetup -> (Text, Turtle.FilePath)
getDir rootDir dirSetup =
  (dname, dPath)
  where
    dname = dirName dirSetup
    dPath = rootDir </> (fromText dname)




setupDir :: Turtle.FilePath -> DirSetup -> IO ()
setupDir rootDir dirSetup = do
  let (dname, dPath) = getDir rootDir dirSetup
  getTemplate rootDir dname dirSetup
  case dirStackType dirSetup of
    FRONT_END -> buildFrontEnd rootDir
    BACK_END -> buildBackEnd rootDir



getTemplate topDir dname dirSetup = do
  majorCommentBlock $ "Setting up " <> dname
  cd topDir
  setupResult <- shell ("git clone " <> gitDir dirSetup <> " " <> dname) empty
  case setupResult of
    ExitSuccess   -> return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)

buildFrontEnd topDir = do
  majorCommentBlock "BUILDING FRONT END"
  let frontEndPath = getDir topDir frontendDirConfig & snd
  putStrLn $ encodeString frontEndPath
  cd frontEndPath
  _ <- shell "yarn install" empty
  _ <- shell "elm-package install --yes" empty
  return ()

buildBackEnd topDir = do
  majorCommentBlock "BUILDING BACK END"
  let backendDir = getDir topDir backendDirConfig & snd
  cd backendDir
  _ <- shell "stack build" empty
  _ <- mkBackendEnv backendDir
  return ()

mkBackendEnv :: Turtle.FilePath -> IO ()
mkBackendEnv backendDir = do
  echo "Enter name of DB"
--   dbName <- readline
--   echo "Enter name of User"
--   dbUser <- readline
--   echo "Enter DB Password"
--   dbPassword <- readline
  let textFile = T.intercalate "\n" [dbHostLine, dbPortLine, dbDatabase, dbSchema, dbUserEnv , dbPasswordEnv ]
  writeTextFile (backendDir </> ".env") textFile

  where
    dbHostLine    = "DB_HOST=localhost"
    dbPortLine    = "DB_PORT=5432"
    dbDatabase    = "DB_DATABASE=test"
    dbSchema      = "DB_SCHEMA=public"
    dbUserEnv     = "DB_USERNAME=postgres"
    dbPasswordEnv = "DB_PASSWORD=postgres"


runFrontEnd topDir = do
  majorCommentBlock "STARTING WEB SERVER"
  cd $ getDir topDir frontendDirConfig & snd
  s <- shell "../ttab yarn start " empty
  case s of
    ExitSuccess   -> do
        printf "\nSuccessfully started server. Go to localhost:3000\n"
        return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)
  return ()

runBackEnd topDir = do
  majorCommentBlock "STARTING LOCAL BACK-END"
  cd $ getDir topDir backendDirConfig & snd
  s <- shell "../ttab ./run.sh" empty

  case s of
    ExitSuccess   -> do
        printf "\nSuccessfully started api. Logs will be output to console\n"
        return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)
  return ()


-- POSSIBLY DEPRECATED

data ValidateResult = Valid | Replace | Keep

validateInitialSetup :: Text -> Turtle.FilePath -> IO ValidateResult
validateInitialSetup dname dPath = do
  existingDir <- testdir dPath
  if existingDir then do
    echo $ unsafeTextToLine $ "Found existing directory " <> dname
    echo "Would you like to replace (R) or keep (enter)"
    answer <- readline
    case answer of
        Just a ->
            case a of
                "R" -> do
                    echo "Replacing existing directories"
                    return Replace
                _ -> do
                  echo "Keeping existing directory"
                  return Keep
  else do
    echo "Detected valid initial state"
    return Valid

validateAndSetupDir :: Turtle.FilePath -> DirSetup -> IO ()
validateAndSetupDir rootDir dirSetup = do
    let (dname, dPath) = getDir rootDir dirSetup
    majorCommentBlock $ "Setting up " <> dname
    validateResult <- validateInitialSetup dname dPath
    case validateResult of
        Valid -> do
            setup
            return ()
        Replace -> do
            rmtree dPath
            setup
        Keep -> return ()
    where
        setup = do
            setupDir rootDir dirSetup
            return ()
