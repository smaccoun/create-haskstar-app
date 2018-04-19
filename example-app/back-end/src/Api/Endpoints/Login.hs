{-# LANGUAGE DeriveAnyClass #-}

module Api.Endpoints.Login where

import           Api.Endpoints.User   (getUserByEmail)
import           App
import           AppPrelude
import           Control.Lens
import qualified Crypto.Scrypt        as S
import           Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.Text            (Text)
import           Data.Text.Encoding   (encodeUtf8)
import           Database.Tables.User as UT (User, UserID, userPassword, userId)
import           Models.Credentials   (Password (..))
import           Models.Login
import           Models.User          (userApiFromUserDB)
import           Servant
import           Servant.Auth.Server

data LoginResponse =
  LoginResponse
    {jwtToken :: Text
    ,userId   :: UserID
    } deriving (Generic, ToJSON)


type LoginAPI = "login"
       :> ReqBody '[JSON] Login :> Post '[JSON] LoginResponse

loginAPI :: Proxy LoginAPI
loginAPI = Proxy


loginServer :: JWTSettings -> ServerT LoginAPI AppM
loginServer jwtSettings =
  loginUserPassword jwtSettings


loginUserPassword :: JWTSettings -> Login -> AppM LoginResponse
loginUserPassword jwtCfg (Login loginEmail loginPassword) = do
  user <- getUserByEmail loginEmail
  if hasCorrectPassword user loginPassword then do
    print (show user :: Text)
    let userApi = userApiFromUserDB user
    eitherJWT <- liftIO $ makeJWT userApi jwtCfg Nothing
    case eitherJWT of
      Left e    -> panic $ show e
      Right jwt -> return $
        LoginResponse
          {jwtToken = decodeUtf8 $ BSL.toStrict jwt
          ,userId = user ^. UT.userId
          }
  else
     throwError err500 {errBody = "Incorrect Password"}

hasCorrectPassword :: User -> Password -> Bool
hasCorrectPassword user (Password password) =
  fst $  S.verifyPass S.defaultParams (S.Pass $ encodeUtf8 password) (user ^. userPassword)
