{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Models.Login where

import           AppPrelude
import           Data.Aeson
import           Models.Credentials

data Login = Login { email :: Email , password :: Password }
   deriving (Eq, Show, Read, Generic)

instance FromJSON Login
