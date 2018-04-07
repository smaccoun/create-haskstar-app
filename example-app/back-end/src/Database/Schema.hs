{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Schema where

import           Control.Lens         hiding (element)
import           Database.Beam
import           Database.Tables.User

data MyAppDb f =
  MyAppDb
    { _users :: f (TableEntity UserT)
    } deriving Generic

makeLenses ''MyAppDb

instance Database be MyAppDb

appDb :: DatabaseSettings be MyAppDb
appDb = defaultDbSettings
