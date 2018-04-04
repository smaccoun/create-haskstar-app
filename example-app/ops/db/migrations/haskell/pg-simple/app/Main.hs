module Main where

import Lib
import Database.PostgreSQL.Simple.Migration

main :: IO (MigrationResult String)
main = do
  putStrLn "INITIALIZING"
  initialize
  putStrLn "MIGRATING"
  simpleMigration
