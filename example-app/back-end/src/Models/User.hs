{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Models.User where

import           AppPrelude
import           Control.Lens         hiding (element)
import           Data.Aeson
import qualified Database.Tables.User as UT (UserID, UserT, userEmail, userId)
import           GHC.Generics         (Generic)
import           Models.Credentials   (Email (..))
import           Servant.Auth.Server

data UserResponse
    = UserResponse
    { _id    :: UT.UserID
    , _email :: Email
    } deriving (Generic)

makeLenses ''UserResponse

deriving instance Show UserResponse
instance ToJSON UserResponse
instance FromJSON UserResponse
instance ToJWT UserResponse
instance FromJWT UserResponse

userApiFromUserDB :: UT.UserT Identity -> UserResponse
userApiFromUserDB userT =
  UserResponse
    {_id = userT ^. UT.userId
    ,_email = Email $ userT ^. UT.userEmail
    }
