module Api.User where

import App
import AppPrelude
import Servant
import DB.Transaction
import Models.User
import Database.Beam
import Database.Beam.Postgres
import DBSchema

type UserAPI =
    "users"
        :> (
            Get '[JSON] [User]
        )

userAPI :: Proxy User
userAPI = Proxy

userServer :: ServerT UserAPI AppM
userServer = getUsers

getUsers :: AppM [User]
getUsers = do
  users <- runQuery $ runSelectReturningList $ select (all_ (_users appDb))
  return users

getUserByEmail :: Text -> AppM User
getUserByEmail email' = do
  userResult <- runQuery $ runSelectReturningOne $
    select $
    do  users <- all_ (_users appDb)
        guard_ (_userEmail users ==. val_ email')
        pure users
  case userResult of
    Just user -> return user
    Nothing -> panic $ "Should only have one user with email" <> email'




