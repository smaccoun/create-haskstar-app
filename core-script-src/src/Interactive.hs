#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Interactive where

import           Data.Maybe
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import           Turtle

prompt :: T.Text -> Maybe T.Text -> IO T.Text
prompt promptQuestion mbDefault = do
  let questionExtension = fmap (\d -> " (default " <> d <> ")") mbDefault & fromMaybe ""
  let question = promptQuestion <> questionExtension <> ": "
  putStrLn "\n-----------------------"
  TIO.putStrLn question
  getAnswer
  where
    getAnswer = do
      answer <- getLine
      if didHitEnter answer then
           case mbDefault of
              Just def -> do
                TIO.putStrLn $ "---> Nothing entered. Using default " <> def
                return def
              Nothing -> getAnswer
      else
        return $ T.pack answer


didHitEnter :: String -> Bool
didHitEnter line =
  T.pack line == T.empty

askToBuild :: IO Bool
askToBuild = do
  majorCommentBlock "Setup complete! You now have a fullstack Haskell setup!"
  answer <- prompt "Would you like to now build the project? (y) yes, (n) no" Nothing
  case answer of
     "y" -> return True
     "n" -> do
        echo "To build the back-end, cd into back-end and run `./run.sh`. For the front-end, cd into front-end and run `yarn start`"
        return False
     _   -> do
        echo "Please entery (y) or (n)"
        return False

printfln :: MonadIO m => Text -> m ()
printfln =
  printf (""%s%" \n")

lineBlockSeperator :: MonadIO m => m ()
lineBlockSeperator =
  printf "***********************************************\n\n"

majorCommentBlock :: (MonadIO m) => Text -> m ()
majorCommentBlock msg = do
  printf "\n\n***********************************************\n"
  printf ("      "%s%"                              \n") msg
  lineBlockSeperator


subCommentBlock :: MonadIO m => Text -> m ()
subCommentBlock msg = do
  printf ("\n\n"%s%"                              \n") msg
  printf "*******************************************************\n\n"


instructionCommentBlock :: MonadIO m => Text -> m ()
instructionCommentBlock msg = do
  printf "*******************************************************\n"
  printf ("*  "%s%"   *\n") msg
  printf "*                                                    *\n"
  printf "*******************************************************\n"

showWelcomeMessage :: IO ExitCode
showWelcomeMessage =
  shell "cat logoAscii.txt" Turtle.empty

showRunInstructions :: MonadIO m => m ()
showRunInstructions = do
  majorCommentBlock "LOCAL RUN INSTRUCTIONS"
  printfln "DB: "
  lineBlockSeperator
  printfln "hasm start db"
  printfln "Back-End: "
  lineBlockSeperator
  printfln "hasm start back-end"
  printfln "Front-End: \n"
  lineBlockSeperator
  printfln "hasm start front-end"
