#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Interactive where

import Turtle
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe

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


majorCommentBlock :: Text -> IO ()
majorCommentBlock msg = do
  printf "\n\n***********************************************\n"
  printf ("      "%s%"                              \n") msg
  printf "***********************************************\n\n"


subCommentBlock :: Text -> IO ()
subCommentBlock msg = do
  printf ("\n\n"%s%"                              \n") msg
  printf "*******************************************************\n\n"


instructionCommentBlock :: Text -> IO ()
instructionCommentBlock msg = do
  printf "*******************************************************\n"
  printf ("*  "%s%"   *\n") msg
  printf "*                                                    *\n"
  printf "*******************************************************\n"

