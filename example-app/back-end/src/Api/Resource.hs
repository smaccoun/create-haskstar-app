{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

module Api.Resource where

import           App
import           GHC.TypeLits
import           Servant


{- Read API -}
type RResourceAPI (resourceName :: Symbol) a i = resourceName :>
  (    Get '[JSON] [a]
  :<|> Capture "id" i    :> Get '[JSON] a
  )

type RResourceServer name a i =
     AppM [a]
  -> (i -> AppM a)
  -> ServerT (RResourceAPI name a i) AppM

rResourceServer :: RResourceServer name a i
rResourceServer listAs getA =
  listAs :<|> getA

{- Create/Read API -}
type CRResourceAPI (resourceName :: Symbol) a i = resourceName :>
  (    Get '[JSON] [a]
  :<|> Capture "id" i    :> Get '[JSON] a
  :<|> ReqBody '[JSON] a :> Post '[JSON] NoContent
  )

crResourceServer
  :: AppM [a]
  -> (i -> AppM a)
  -> (a -> AppM NoContent)
  -> ServerT (CRResourceAPI name a i) AppM
crResourceServer listAs getA postA =
  listAs :<|> getA :<|> postA
