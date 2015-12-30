module Types where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader


type App a = ReaderT AppConfig (ExceptT AppError IO) a


data AppError = LoginError String
              | BodyNotParsable
              | UsageNotFound
              | UsageNotExtractable
              | PublisherError String
              deriving Show


data AppArgs = ShowVersion
             | Run AppConfig
               deriving Show


data AppConfig = AppConfig {
      acProviderLogin    :: ProviderLogin
    , acPublishEndpoints :: Endpoints
    } deriving Show


data ProviderLogin = ProviderLogin {
      plUser :: String
    , plPass :: String
    } deriving Show


type Endpoints = [Endpoint]
data Endpoint = EndpointQuota String
              | EndpointUsed String
              | EndpointAvailable String
                deriving Show


data Usage = Usage {
      uQuota     :: Int
    , uUsed      :: Int
    , uAvailable :: Int
    } deriving Show
