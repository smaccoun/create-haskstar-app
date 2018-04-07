{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Config.AppConfig (
        module Config.ServerConfig
       ,module Config.DBConfig
     ) where

import           Config.DBConfig
import           Config.ServerConfig
