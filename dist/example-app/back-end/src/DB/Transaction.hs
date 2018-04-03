{-# LANGUAGE ConstraintKinds #-}

module DB.Transaction where

import           App
import           AppPrelude
import           Database.Beam              (MonadBeam, withDatabase)
import qualified Database.PostgreSQL.Simple as PGS

type MonadQuery syntax be m = MonadBeam syntax be PGS.Connection m

runQuery :: MonadQuery syntax be m => m a -> AppM a
runQuery query' = do
  conn <- getConn
  liftIO $ withDatabase conn query'
