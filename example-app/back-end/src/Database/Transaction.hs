{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes      #-}

module Database.Transaction where

import           App
import           AppPrelude
import           Database.Beam                   (MonadBeam, withDatabase)
import           Database.Beam.Backend
import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Query
import qualified Database.PostgreSQL.Simple      as PGS

type MonadQuery syntax be m = MonadBeam syntax be PGS.Connection m
type AppQ cmd a = SqlSelect (Sql92SelectSyntax cmd) a
type SqlFlavorConstraint cmd be m a = (IsSql92Syntax cmd, MonadQuery cmd be m, FromBackendRow be a)

runSql :: MonadQuery syntax be m => PGPool -> m a -> IO a
runSql pool query' = do
  conn <- getIOConnFromPool pool
  withDatabase conn query'

runSqlM :: MonadQuery syntax be m => m a -> AppM a
runSqlM query' = do
  Config{..} <- ask
  liftIO $ runSql getPool query'

runQuery :: SqlFlavorConstraint cmd be m a => PGPool -> AppQ cmd a -> IO [a]
runQuery pool query' = do
  runSql pool (runSelectReturningList query')

runQueryM :: SqlFlavorConstraint cmd be m a => AppQ cmd a -> AppM [a]
runQueryM query' = do
  Config{..} <- ask
  liftIO $ runQuery getPool query'


runQuerySingle :: SqlFlavorConstraint cmd be m a => PGPool -> AppQ cmd a -> IO a
runQuerySingle pool query' = do
  result <- runQuery pool query'
  case result of
    []    -> panic "No results found"
    [x]   -> return x
    (_:_) -> panic "More than one result found"

runQuerySingleM :: SqlFlavorConstraint cmd be m a => AppQ cmd a -> AppM a
runQuerySingleM query' = do
  Config{..} <- ask
  liftIO $ runQuerySingle getPool query'
