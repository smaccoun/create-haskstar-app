module Api.Endpoints.Login where

import           Api.Endpoints.User   (getUserByEmail)
import           App
import           AppPrelude
import           Control.Lens
import qualified Crypto.Scrypt        as S
import qualified Data.ByteString.Lazy as BSL
import           Data.Text            (Text)
import           Data.Text.Encoding   (encodeUtf8)
import           Database.Tables.User (User, userPassword)
import           Models.Credentials   (Password (..))
import           Models.Login
import           Models.User          (userApiFromUserDB)
import           Servant
import           Servant.Auth.Server

type LoginAPI = "login"
       :> ReqBody '[JSON] Login :> Post '[JSON] Text

loginAPI :: Proxy LoginAPI
loginAPI = Proxy

loginServer :: JWTSettings -> Login -> AppM Text
loginServer jwtCfg (Login loginEmail loginPassword) = do
  user <- getUserByEmail loginEmail
  if hasCorrectPassword user loginPassword then do
    print (show user :: Text)
    let userApi = userApiFromUserDB user
    eitherJWT <- liftIO $ makeJWT userApi jwtCfg Nothing
    case eitherJWT of
      Left e    -> panic $ show e
      Right jwt -> return $ decodeUtf8 $ BSL.toStrict jwt
  else
     return "Bad Password"

hasCorrectPassword :: User -> Password -> Bool
hasCorrectPassword user (Password password) =
  fst $  S.verifyPass S.defaultParams (S.Pass $ encodeUtf8 password) (user ^. userPassword)
