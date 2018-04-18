{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Api.API where

import           Api.Endpoints.Login
import           Api.Endpoints.User
import           App
import           AppPrelude
import           Data.Swagger                        (Swagger, ToSchema)
import           Data.Text                           (Text)
import           Models.Credentials                  (Email, Password)
import           Models.Login
import           Models.User                         (UserResponse (..))
import           Servant
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import           Servant.Swagger

---------------------------------------------------------------
type Protected
   = UserAPI

protected :: AuthResult UserResponse -> ServerT Protected AppM
protected (Authenticated user) =
  userServer user

protected _ = throwAll err401

type Unprotected =
       "health" :> Get '[JSON] Text
  :<|> LoginAPI

unprotectedProxy :: Proxy Unprotected
unprotectedProxy = Proxy

unprotected :: JWTSettings -> ServerT Unprotected AppM
unprotected jwts =
       return "Okay"
  :<|> loginServer jwts

type API auths =
       (Auth auths UserResponse :> Protected)
  :<|> Unprotected

api :: Proxy (API '[JWT])
api = Proxy


serverAPI :: JWTSettings -> ServerT (API auths) AppM
serverAPI jwts =
       protected
  :<|> unprotected jwts


-- SWAGGER

swaggerUnprotected :: Swagger
swaggerUnprotected = toSwagger unprotectedProxy

instance ToSchema Login
instance ToSchema Email
instance ToSchema Password
