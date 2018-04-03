{-# LANGUAGE DeriveGeneric        #-}

module DBSchema where

import           Database.Beam
import Models.User

data MyAppDb f =
  MyAppDb
    { _users :: f (TableEntity UserT)
    } deriving Generic

instance Database be MyAppDb

appDb :: DatabaseSettings be MyAppDb
appDb = defaultDbSettings
