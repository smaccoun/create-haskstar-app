#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Turtle

main :: IO ()
main = do
  chmod executable "./ttab"
  topDir <- pwd
  validateAndSetupDir topDir backendDirConfig
  validateAndSetupDir topDir frontendDirConfig
  askToRun $ runServers topDir
  cd topDir
  return ()
