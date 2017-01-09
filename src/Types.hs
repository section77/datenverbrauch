{-# LANGUAGE TypeSynonymInstances #-}
module Types where

import           Protolude


type App a = ReaderT AppConfig (ExceptT AppError IO) a


data AppError = LoginError Text
              | TagNotFound Text
              | TagHasNoText Text
              | UsageNotParsable Text
              | BalanceNotParsable Text
              | UsageNotExtractable
              | EndDateNotFound
              | EndDateNotParsable Text
              | PublisherError Text
              deriving Show


data AppArgs = ShowVersion
             | Run AppConfig
               deriving Show

type ProviderBaseUrl = Text

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
      plUser :: Text
    , plPass :: Text
    } deriving Show


type Endpoints = [Endpoint]
data Endpoint = EndpointQuota Text
              | EndpointUsed Text
              | EndpointAvailable Text
              | EndpointBalance Text
              | EndpointDaysLeft Text
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
