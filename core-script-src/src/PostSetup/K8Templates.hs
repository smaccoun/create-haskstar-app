{-# LANGUAGE OverloadedStrings #-}

module PostSetup.K8Templates where

import qualified Data.Aeson as A
import           Data.Text  (Text)
import           Lib        (AppName (..), Domain (..), Email (..),
                             RemoteDockerImage (..))
import           Turtle

data StacheTemplate =
    StacheTemplate
      {stacheFilename :: Text
      ,configObj      :: A.Value
      }

frontendDeploymentConfig :: AppName -> RemoteDockerImage -> StacheTemplate
frontendDeploymentConfig (AppName appName') (RemoteDockerImage remoteDockerImage') =
    StacheTemplate
      {stacheFilename = "frontend-deployment.yaml.mustache"
      ,configObj =
        A.object
          [ "appName" A..= appName'
          , "remoteDockerImage" A..= remoteDockerImage'
          ]
      }

backendDeploymentConfig :: AppName -> RemoteDockerImage -> StacheTemplate
backendDeploymentConfig (AppName appName') (RemoteDockerImage remoteDockerImage') =
    StacheTemplate
      {stacheFilename = "backend-deployment.yaml.mustache"
      ,configObj =
        A.object
          [ "appName" A..= appName'
          , "remoteDockerImage" A..= remoteDockerImage'
          ]
      }

ingressConfig :: AppName -> Domain -> StacheTemplate
ingressConfig (AppName appName') (Domain domain) =
    StacheTemplate
      {stacheFilename = "ingress.yaml.mustache"
      ,configObj =
        A.object
          [ "appName" A..= appName'
          , "domain" A..= domain
          ]
      }

certIssuerConfig :: Email -> StacheTemplate
certIssuerConfig (Email email) =
    StacheTemplate
      {stacheFilename = "acme-staging-issuer.yaml.mustache"
      ,configObj =
        A.object
          [ "userEmail" A..= email
          ]
      }
