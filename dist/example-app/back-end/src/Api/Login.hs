module Api.Login where

import App
import AppPrelude
import Api.User (getUserByEmail)
import Models.User (User)
import Data.Text (Text)
import Data.Aeson
import Data.Text.Encoding (decodeUtf8)
import Servant.Auth.Server
import Servant
import qualified Data.ByteString.Lazy as BSL

data Login = Login { email :: Text , password :: Text }
   deriving (Eq, Show, Read, Generic)


instance FromJSON Login
instance ToJWT User
instance FromJWT User

type LoginAPI = "login"
       :> ReqBody '[JSON] Login :> Post '[JSON] Text

loginAPI :: Proxy LoginAPI
loginAPI = Proxy

loginServer :: JWTSettings -> Login -> AppM Text
loginServer jwtCfg (Login email _) = do
  user <- getUserByEmail email
  print $ (show user :: Text)
  eitherJWT <- liftIO $ makeJWT user jwtCfg Nothing
  case eitherJWT of
    Left e -> panic $ show e
    Right jwt -> return $ decodeUtf8 $ BSL.toStrict jwt
