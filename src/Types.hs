{-# LANGUAGE TypeSynonymInstances #-}
module Types where

import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader


type App a = ReaderT AppConfig (ExceptT AppError IO) a


data AppError = LoginError String
              | BodyNotParsable
              | BalanceNotFound
              | BalanceNotExtractable
              | UsageNotExtractable
              | PublisherError String
              deriving Show


data AppArgs = ShowVersion
             | Run AppConfig
               deriving Show


data AppConfig = AppConfig {
      acQuiet              :: Bool
    , acProviderLogin      :: ProviderLogin
    , acPublishEndpoints   :: Endpoints
    , acAvailableThreshold :: AvailableThreshold
    , acBalanceThreshold   :: BalanceThreshold
    } deriving Show


data ProviderLogin = ProviderLogin {
      plUser :: String
    , plPass :: String
    } deriving Show


type Endpoints = [Endpoint]
data Endpoint = EndpointQuota String
              | EndpointUsed String
              | EndpointAvailable String
              | EndpointBalance String
                deriving Show



data AvailableThreshold = AvailableThreshold {
                        atWarning  :: Maybe Int
                      , atCritical :: Maybe Int
                      } deriving Show

data BalanceThreshold = BalanceThreshold {
                          btWarning  :: Maybe Balance
                        , btCritical :: Maybe Balance
                        } deriving Show

type Balance = Float
data Tariff = Tariff {
      tBalance :: Balance
    , tUsage   :: Usage
    } deriving Show


data Usage = UsageNotAvailable
           | Usage {
               uQuota     :: Int
             , uUsed      :: Int
             , uAvailable :: Int
             } deriving Show




class IsBelowThreshold a where
    isBelowWarning :: a -> ReaderT AppConfig IO Bool

    isBelowCritical :: a -> ReaderT AppConfig IO Bool


instance IsBelowThreshold Usage where

    isBelowWarning UsageNotAvailable = pure False
    isBelowWarning (Usage _ _ a) = do
                                  n <- atWarning <$> asks acAvailableThreshold
                                  pure $ maybe False (> a) n


    isBelowCritical UsageNotAvailable = pure False
    isBelowCritical (Usage _ _ a) = do
                                  w <- atCritical <$> asks acAvailableThreshold
                                  pure $ maybe False (> a) w



instance IsBelowThreshold Balance where

    isBelowWarning b = do
      n <- btWarning <$> asks acBalanceThreshold
      pure $ maybe False (> b) n


    isBelowCritical b = do
      w <- btCritical <$> asks acBalanceThreshold
      pure $ maybe False (> b) w
