{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveDataTypeable         #-}

module Lib where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Migration
import System.Envy
import GHC.Generics

------------------------------------------------------------------------------
data PGConnect = PGConnect {
      pgUser :: String
    , pgPass :: String
    , pgDB   :: String
  } deriving (Generic, Show)

instance FromEnv PGConnect where
  fromEnv =
     PGConnect  <$> env "POSTGRES_USER"
        <*> env "POSTGRES_PASSWORD"
        <*> env "POSTGRES_DB"

mapConInfo :: PGConnect -> ConnectInfo
mapConInfo (PGConnect _ pgPass pgDB) =
  defaultConnectInfo { connectPassword = pgPass, connectDatabase=pgDB }

getConnInfo :: IO (ConnectInfo)
getConnInfo = do
  env <- decodeEnv
  let asPGS = fmap mapConInfo env
  case asPGS of
    Left e -> error $ show e
    Right conInfo -> return conInfo

initialize :: IO (MigrationResult String)
initialize = do
  conInfo <- getConnInfo
  con <- connect conInfo
  withTransaction con $ runMigration $
        MigrationContext MigrationInitialization True con


simpleMigration :: IO (MigrationResult String)
simpleMigration = do
    let dir = "./migrations"
    conInfo <- getConnInfo
    con <- connect conInfo
    withTransaction con $ runMigration $
        MigrationContext (MigrationDirectory dir) True con

