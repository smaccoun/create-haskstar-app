#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Run where

import           Context
import           Data.Text                 (pack)
import           Filesystem.Path.CurrentOS (encodeString)
import           Distribution.System
import           Interactive
import           Turtle

runFrontEnd :: App ()
runFrontEnd = do
  liftIO $ majorCommentBlock "STARTING WEB SERVER"
  fromAppRootDir
  curOS' <- getCurOS
  cd "front-end"
  let runCmd = "yarn start"
  s <-
    case curOS' of
      OSX -> runWithTTab runCmd
      _ -> runAsBackground runCmd
  case s of
    ExitSuccess -> do
        liftIO $ printf ("\nSuccessfully started server. Go to localhost:3000\n")
        return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)


runBackEnd :: App ()
runBackEnd = do
  liftIO $ majorCommentBlock "STARTING LOCAL BACK-END"
  fromAppRootDir
  cd "back-end"
  s <- runWithTTab "./run.sh"
  case s of
    ExitSuccess   -> liftIO $ do
        printf "\nSuccessfully started api. Logs will be output to console\n"
        return ()
    ExitFailure n -> die (" failed with exit code: " <> repr n)
  return ()


runServers :: App ()
runServers = do
    runBackEnd
    runFrontEnd

askToRun :: App () -> App ()
askToRun onYes = do
  answer <- liftIO $ prompt "Setup Complete! Would you like to boot up the servers? (y) yes, (n) no" Nothing
  case answer of
     "y" -> onYes
     "n" -> liftIO $ echo "You can boot up each server by running ./run.sh"
     _   -> liftIO $ echo "Please entery (y) or (n)"


runWithTTab :: Text -> App ExitCode
runWithTTab cmd = do
  ttabBaseCmd <- getTTab
  let asTextTTab = pack $ encodeString ttabBaseCmd
      ttabCmd =  asTextTTab <> " " <> cmd
  s <- liftIO $ shell ttabCmd empty
  return s

runAsBackground :: Text -> App ExitCode
runAsBackground cmd = do
  let asBackground = cmd <> " &"
  s <- liftIO $ shell asBackground empty
  return s



