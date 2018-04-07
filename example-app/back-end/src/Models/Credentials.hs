{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}

module Models.Credentials where

import           AppPrelude
import           Control.Lens (makeLenses)
import           Data.Aeson
import           Data.Text    (Text)

newtype Email = Email Text
    deriving (Generic, Eq, Show, Read, FromJSON, ToJSON)

makeLenses ''Email

newtype Password = Password Text
    deriving (Generic, Eq, Show, Read, FromJSON, ToJSON)
