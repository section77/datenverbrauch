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
      acProviderLogin    :: ProviderLogin
    , acPublishEndpoints :: Endpoints
    , acUsageThreshold   :: UsageThreshold
    , acBalanceThreshold :: BalanceThreshold
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



data UsageThreshold = UsageThreshold {
                        utNotification :: Maybe Int
                      , utWarning      :: Maybe Int
                      } deriving Show

data BalanceThreshold = BalanceThreshold {
                          btNotification :: Maybe Balance
                        , btWarning      :: Maybe Balance
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
    isBelowNotification :: a -> ReaderT AppConfig IO Bool

    isBelowWarning :: a -> ReaderT AppConfig IO Bool


instance IsBelowThreshold Usage where

    isBelowNotification UsageNotAvailable = pure False
    isBelowNotification (Usage _ _ a) = do
                                  n <- utNotification <$> asks acUsageThreshold
                                  pure $ maybe False (> a) n


    isBelowWarning UsageNotAvailable = pure False
    isBelowWarning (Usage _ _ a) = do
                                  w <- utWarning <$> asks acUsageThreshold
                                  pure $ maybe False (> a) w



instance IsBelowThreshold Balance where

    isBelowNotification b = do
      n <- btNotification <$> asks acBalanceThreshold
      pure $ maybe False (> b) n


    isBelowWarning b = do
      w <- btWarning <$> asks acBalanceThreshold
      pure $ maybe False (> b) w
