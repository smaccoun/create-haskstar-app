{-# LANGUAGE OverloadedStrings #-}

module DBConfig where

import qualified Data.Text   as T
import           Interactive
import           Text.Regex
import           Turtle

data DBConfig =
  DBConfig
    {host       :: T.Text
    ,port       :: Int
    ,dbName     :: T.Text
    ,dbUser     :: T.Text
    ,dbPassword :: T.Text
    ,dbSchema   :: T.Text
    }


mkDBConfig :: Text -> DBConfig
mkDBConfig appName =
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

promptDBConfig :: IO DBConfig
promptDBConfig = do
  instructionCommentBlock "First setup the DB configuration you would like"
  dbName <- prompt "Enter name of DB" Nothing
  dbSchema <- prompt "Enter default schema" (Just "public")
  dbUser <- prompt "Enter name of User" (Just "postgres")
  dbPassword <- prompt "Enter DB Password" (Just "postgres")
  return $
    DBConfig
      {host = "localhost"
      ,port = 5432
      ,dbName = dbName
      ,dbUser = dbUser
      ,dbPassword = dbPassword
      ,dbSchema = dbSchema
      }


getDBEnvFile :: DBConfig -> Turtle.FilePath -> T.Text
getDBEnvFile (DBConfig host port dbName dbUser dbPassword dbSchema) backendDir =
  T.intercalate "\n" $
         [ dbDatabaseLn dbName
         , dbUserLn dbUser
         , dbPasswordLn dbPassword
         ]
  where
    dbPasswordLn password = "POSTGRES_PASSWORD=" <> dbPassword
    dbDatabaseLn dbName   = "POSTGRES_DB=" <> dbName
    dbUserLn dbUser = "POSTGRES_USER=" <> dbUser
    dbPortLn dbPort = "POSTGRES_PORT=" <> show port
