{-# LANGUAGE TypeSynonymInstances #-}
module Types where

import           BasicPrelude
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader


type App a = ReaderT AppConfig (ExceptT AppError IO) a


data AppError = LoginError Text
              | BalanceNotFound
              | UsageNotExtractable
              | EndDateNotFound
              | EndDateNotParsable String
              | PublisherError String
              deriving Show


data AppArgs = ShowVersion
             | Run AppConfig
               deriving Show

type ProviderBaseUrl = String

data AppConfig = AppConfig {
      acQuiet              :: Bool
    , acProviderLogin      :: ProviderLogin
    , acPersistPath        :: Maybe FilePath
    , acPublishEndpoints   :: Endpoints
    , acAvailableThreshold :: AvailableThreshold
    , acBalanceThreshold   :: BalanceThreshold
    , acProviderBaseUrl    :: ProviderBaseUrl
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
              | EndpointDaysLeft String
                deriving Show



data AvailableThreshold = AvailableThreshold {
                        atWarning  :: Maybe Int
                      , atCritical :: Maybe Int
                      } deriving Show

data BalanceThreshold = BalanceThreshold {
                          btWarning  :: Maybe Float
                        , btCritical :: Maybe Float
                        } deriving Show


data Tariff = Tariff {
      tBalance  :: Balance
    , tUsage    :: Usage
    , tDaysLeft :: Integer
    } deriving Show


data Balance = BalanceNotAvailable
             | Balance Float deriving Show


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

    isBelowWarning BalanceNotAvailable = pure False
    isBelowWarning (Balance b) = do
      n <- btWarning <$> asks acBalanceThreshold
      pure $ maybe False (> b) n


    isBelowCritical BalanceNotAvailable = pure False
    isBelowCritical (Balance b) = do
      w <- btCritical <$> asks acBalanceThreshold
      pure $ maybe False (> b) w
