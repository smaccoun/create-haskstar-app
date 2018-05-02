{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DBConfig where

import qualified Data.Text    as T
import           GHC.Generics
import           Interactive
import qualified System.Envy  as SE
import           Text.Regex
import           Turtle

data DBConfig =
  DBConfig
    {host       :: T.Text
    ,port       :: Integer
    ,dbName     :: T.Text
    ,dbUser     :: T.Text
    ,dbPassword :: T.Text
    ,dbSchema   :: T.Text
    }


mkDefaultLocalDBConfig :: Text -> DBConfig
mkDefaultLocalDBConfig appName =
    DBConfig
      {host = "localhost"
      ,port = 6543
      ,dbName = T.replace "-" "_" appName
      ,dbUser = "postgres"
      ,dbPassword = "postgres"
      ,dbSchema = "public"
      }

showDBInfo :: DBConfig -> IO ()
showDBInfo (DBConfig host port dbName dbUser dbPassword dbSchema) =
  subCommentBlock $ "Spinning up a local db instance in Docker with DB name " <> dbName <> " on port " <> (T.pack $ show port) <> " with username " <> dbUser <> " and password " <> dbPassword


textForDBEnvFile :: DBConfig -> T.Text
textForDBEnvFile (DBConfig host port dbName dbUser dbPassword dbSchema) =
  T.intercalate "\n" $
         [ dbDatabaseLn dbName
         , dbUserLn dbUser
         , dbPasswordLn dbPassword
         , dbPortLn port
         , dbHostLn
         ]
  where
    dbPasswordLn password = "POSTGRES_PASSWORD=" <> dbPassword
    dbDatabaseLn dbName   = "POSTGRES_DB=" <> dbName
    dbUserLn dbUser = "POSTGRES_USER=" <> dbUser
    dbPortLn dbPort = T.pack $ "POSTGRES_PORT=" <> show port
    dbHostLn = T.pack $ "POSTGRES_HOST=localhost"


data PGConfig =
  PGConfig
    {postgresHost     :: Text
    ,postgresDB       :: Text
    ,postgresUser     :: Text
    ,postgresPassword :: Text
    ,postgresPort     :: Integer
    } deriving (Generic, Show)

instance SE.FromEnv PGConfig where
  fromEnv =
    PGConfig
    <$> SE.env "POSTGRES_HOST"
    <*> SE.env "POSTGRES_DB"
    <*> SE.env "POSTGRES_USER"
    <*> SE.env "POSTGRES_PASSWORD"
    <*> SE.env "POSTGRES_PORT"

instance SE.ToEnv PGConfig where
  toEnv PGConfig{..} = SE.makeEnv
       [ "POSTGRES_HOST" SE..= postgresHost
       , "POSTGRES_PORT" SE..= postgresPort
       , "POSTGRES_USER" SE..= postgresUser
       , "POSTGRES_PASSWORD" SE..= postgresPassword
       , "POSTGRES_DB"   SE..= postgresDB
       ]


dbConfigToPGConfig :: DBConfig -> PGConfig
dbConfigToPGConfig DBConfig{..} =
  PGConfig
    {postgresHost = host
    ,postgresDB = dbName
    ,postgresUser = dbUser
    ,postgresPassword = dbPassword
    ,postgresPort = port
    }
