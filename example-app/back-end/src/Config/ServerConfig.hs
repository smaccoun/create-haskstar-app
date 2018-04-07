{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Config.ServerConfig where

import           Data.Default
import qualified Network.Wai                          as Wai
import           Network.Wai.Middleware.Cors
import qualified Network.Wai.Middleware.RequestLogger as MidRL
import qualified System.Log.FastLogger                as FL

import           App
import           AppPrelude

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
