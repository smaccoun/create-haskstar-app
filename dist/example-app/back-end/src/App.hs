module App where

import           AppPrelude                 hiding (traceIO)

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Maybe                 (fromMaybe)
import           Data.Pool
import           Data.Text                  (pack, unpack)
import           Data.Text                  (Text)
import qualified Database.PostgreSQL.Simple as PGS
import qualified Servant
import           System.Environment         (lookupEnv)
import           System.Log.FastLogger      (LoggerSet, pushLogStrLn, toLogStr)
import qualified System.Log.FastLogger      as FL
import           Text.Read                  (Read, readMaybe)
import Crypto.JOSE.JWK (JWK)

data Environment
  = Test
  | Development
  | Production
  deriving (Show, Eq, Read)

data LogTo
  = STDOut
  | STDErr
  | File Text
  deriving (Show, Eq, Read)

data Config = Config
  { getLogger :: LoggerSet
  , getPool   :: Pool PGS.Connection
  , getAuthKey :: JWK
  }

type AppM = ReaderT Config Servant.Handler


addToLogger :: Text -> AppM ()
addToLogger message =
  AppPrelude.ask >>= \cfg -> liftIO $ pushLogStrLn (getLogger cfg) (toLogStr message)

makeLogger :: LogTo -> IO LoggerSet
makeLogger logTo = case logTo of
        STDOut        -> FL.newStdoutLoggerSet FL.defaultBufSize
        STDErr        -> FL.newStderrLoggerSet FL.defaultBufSize
        File filename -> FL.newFileLoggerSet FL.defaultBufSize $ unpack filename

getConnFromPool :: Pool PGS.Connection -> AppM PGS.Connection
getConnFromPool pool = withResource pool return


getConn :: AppM PGS.Connection
getConn = ask >>= getConnFromPool . getPool


mkPool :: PGS.ConnectInfo -> IO (Pool PGS.Connection)
mkPool con =
  createPool start PGS.close 10 (0.5) 10
    where
      start = PGS.connect con

envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8


lookupEnvStrDefault :: Text -> Text -> IO Text
lookupEnvStrDefault var def = do
  env <- lookupEnv . unpack $ var
  return (fromMaybe def (pack <$> env))


lookupEnvDefault :: Read a => Text -> a -> IO a
lookupEnvDefault var def = do
  env <- lookupEnv . unpack $ var
  return (fromMaybe def $ env >>= readMaybe)


lookupEnvOrError :: Text -> IO Text
lookupEnvOrError var = do
  env <- lookupEnv . unpack $ var
  case env of
    Just e  -> return $ pack e
    Nothing -> panic $ "Could not read environment variable: " <> var
