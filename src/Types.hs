module Types where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader


type App a = ReaderT AppConfig (ExceptT AppError IO) a


data AppError = LoginError String
              | InvalidResponseBody String
              deriving Show


data AppArgs = ShowVersion
             | Run AppConfig
               deriving Show

data AppConfig = AppConfig {
      acProviderLogin :: ProviderLogin
    } deriving Show

data ProviderLogin = ProviderLogin {
      plUser :: String
    , plPass :: String
    } deriving Show
