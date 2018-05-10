{-# LANGUAGE OverloadedStrings #-}

module PostSetup.K8Templates where

import qualified Data.Aeson as A
import           Turtle

data StacheTemplate =
    StacheTemplate
      {stacheFilename :: Turtle.FilePath
      ,configObj      :: A.Value
      }

frontendDeploymentConfig :: Text -> Text -> StacheTemplate
frontendDeploymentConfig appName' remoteDockerImage' =
    StacheTemplate
      {stacheFilename = "frontend-deployment.yaml.mustache"
      ,configObj =
        A.object
          [ "appName" A..= appName'
          , "remoteDockerImage" A..= remoteDockerImage'
          ]
      }

backendDeploymentConfig :: Text -> Text -> StacheTemplate
backendDeploymentConfig appName' remoteDockerImage' =
    StacheTemplate
      {stacheFilename = "backend-deployment.yaml.mustache"
      ,configObj =
        A.object
          [ "appName" A..= appName'
          , "remoteDockerImage" A..= remoteDockerImage'
          ]
      }
