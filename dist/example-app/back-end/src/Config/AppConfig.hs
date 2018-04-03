{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Config.AppConfig where

import           Data.Default
import qualified Network.Wai                          as Wai
import           Network.Wai.Middleware.Cors
import qualified Network.Wai.Middleware.RequestLogger as MidRL
import qualified System.Log.FastLogger                as FL

import           App
import           AppPrelude
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple           as PGS
import           Prelude                              (read)

makeMiddleware :: FL.LoggerSet -> Environment -> IO Wai.Middleware
makeMiddleware logger _ =
          combineMiddleware corsified
        $ MidRL.mkRequestLogger
        $ def { MidRL.destination = MidRL.Logger logger }

corsified :: Wai.Middleware
corsified = cors (const $ Just appCorsResourcePolicy)

combineMiddleware :: Wai.Middleware -> IO Wai.Middleware -> IO Wai.Middleware
combineMiddleware a = fmap (. a)

appCorsResourcePolicy :: CorsResourcePolicy
appCorsResourcePolicy = CorsResourcePolicy {
    corsOrigins        = Nothing
  , corsMethods        = ["OPTIONS", "GET", "PUT", "POST", "PATCH", "DELETE"]
  , corsRequestHeaders = ["Authorization", "Content-Type"]
  , corsExposedHeaders = Nothing
  , corsMaxAge         = Nothing
  , corsVaryOrigin     = False
  , corsRequireOrigin  = False
  , corsIgnoreFailures = False
}


data DBConfig =
  DBConfig
    {dbHost     :: T.Text
    ,dbPort     :: Int
    ,dbDatabase :: T.Text
    ,dbSchema   :: Maybe T.Text
    ,dbUsername :: T.Text
    ,dbPassword :: T.Text
    }


getDBConnectionInfo :: Environment -> IO DBConfig
getDBConnectionInfo _ = do
  dbHost'      <- lookupEnvOrError "DB_HOST"
  dbPort'      <- lookupEnvOrError "DB_PORT"
  dbDatabase'  <- lookupEnvOrError "DB_DATABASE"
  dbSchema'    <- lookupEnvOrError "DB_SCHEMA"
  dbUsername'  <- lookupEnvOrError "DB_USERNAME"
  dbPassword'  <- lookupEnvOrError "DB_PASSWORD"

  return $
    DBConfig
      {dbHost      = dbHost'
      ,dbPort      = read (T.unpack dbPort')
      ,dbDatabase  = dbDatabase'
      ,dbSchema    = Just dbSchema'
      ,dbUsername  = dbUsername'
      ,dbPassword  = dbPassword'
      }


connInfoToPG :: DBConfig -> PGS.ConnectInfo
connInfoToPG connInfo = PGS.defaultConnectInfo
                        { PGS.connectHost = T.unpack . dbHost $ connInfo
                        , PGS.connectUser = T.unpack . dbUsername $ connInfo
                        , PGS.connectPassword = T.unpack . dbPassword $ connInfo
                        , PGS.connectDatabase = T.unpack . dbDatabase $ connInfo
                        }
