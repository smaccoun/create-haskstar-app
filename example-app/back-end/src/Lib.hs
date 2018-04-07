{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( startApp
    ) where

import           App
import           AppPrelude
import           Control.Monad.Except       (catchError)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Natural            ((:~>) (NT))
import qualified Crypto.JOSE                as Jose
import qualified Data.Aeson                 as A
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.Text                  as T
import qualified Network.Wai                as Wai
import qualified Network.Wai.Handler.Warp   as Warp
import           Servant                    as S
import           Servant                    ((:<|>))
import           Servant.Auth.Server        (JWT, JWTSettings,
                                             defaultCookieSettings,
                                             defaultJWTSettings)
import qualified Servant.Swagger.UI         as SUI
import qualified System.Log.FastLogger      as FL

import           Api.API
import           Config.AppConfig


startApp :: [[Char]] -> IO ()
startApp charArgs = do
    let args = fmap T.pack charArgs
    env  <- lookupEnvDefault "SERVANT_ENV" Development
    port <- lookupEnvDefault "SERVANT_PORT" 8080

    (config, logTo) <- setAppConfig env args
    let logger = getLogger config

    midware <- makeMiddleware logger env
    let initialLogMsg = intercalate " " $ ["Listening on port", show port, "at level", show env, "and logging to", (show logTo), "with args", T.unpack (T.unwords args), "\n"]
    FL.pushLogStr logger $ FL.toLogStr initialLogMsg
    Warp.run port
      $ midware
      $ app config

setAppConfig :: Environment -> [Text] -> IO (App.Config, LogTo)
setAppConfig env args = do
    pool <- getDBConnection env
    logTo <- case listToMaybe args of
      Just filename -> return $ File filename
      Nothing       -> lookupEnvDefault "SERVANT_LOG" STDOut
    logger  <- makeLogger logTo

    jwkJsonString <- fmap (LBS.fromStrict . encodeUtf8) $ lookupEnvOrError "AUTH_JWK"
    let jwkJson = (A.decode jwkJsonString :: Maybe Jose.JWK)
        jwk = fromMaybe (panic "BAD JWK") jwkJson


    return (Config logger pool jwk, logTo)

getDBConnection :: Environment -> IO PGPool
getDBConnection env = do
    dbConfig <- getDBConnectionInfo  env
    mkPool $ connInfoToPG dbConfig


app :: App.Config -> Wai.Application
app config@(Config _ _ authKey) = do
    let jwtCfg = defaultJWTSettings authKey
        cfg = defaultCookieSettings S.:. jwtCfg S.:. S.EmptyContext
        apiWithDocs = (Proxy :: Proxy ApiWithDocs)
    S.serveWithContext apiWithDocs cfg (serverWithDocs config jwtCfg)

appServer :: App.Config -> JWTSettings ->  S.Server (API auths)
appServer config jwtConfig =
  S.enter (NT $ runCustomHandler config) (serverAPI jwtConfig)

type ApiWithDocs =
       API '[JWT]
  :<|> SwaggerAPI

type SwaggerAPI = SUI.SwaggerSchemaUI "swagger-ui" "swagger.json"

serverWithDocs :: App.Config -> JWTSettings ->  S.Server ApiWithDocs
serverWithDocs config jwtConfig =
       appServer config jwtConfig
  :<|> SUI.swaggerSchemaUIServer swaggerUnprotected


runCustomHandler :: Config -> AppM a -> S.Handler a
runCustomHandler config handler =
  catchError (nt config handler) errorHandler
  where
    errorHandler :: S.ServantErr -> S.Handler a
    errorHandler err = errorHandler' err (S.errHTTPCode err)

    errorHandler' :: S.ServantErr -> Int -> S.Handler a
    errorHandler' err _ =
      S.throwError err

nt :: App.Config -> AppM a -> S.Handler a
nt s x = runReaderT x s