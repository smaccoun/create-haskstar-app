{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Models.Credentials where

import           AppPrelude
import           Control.Lens
import           Data.Aeson
import           Data.Text    (Text)

newtype Email =
  Email {_unEmail :: Text}
    deriving (Generic, Eq, Show, Read, FromJSON, ToJSON)

makeLenses ''Email

newtype Password =
  Password {unPassword :: Text}
    deriving (Generic, Eq, Show, Read, FromJSON, ToJSON)
