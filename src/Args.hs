{-# LANGUAGE OverloadedStrings #-}
module Args (appArgs)  where

import qualified Data.Text                 as T
import           Options.Applicative
import           Options.Applicative.Types (readerAsk)
import           Protolude

import           Types

appArgs :: Parser AppArgs
appArgs = flag' ShowVersion (short 'v' <> long "version" <> help "app version")
          <|> Run <$> (   AppConfig
                      <$> quiet
                      <*> providerLogin
                      <*> persistPath
                      <*> endpoints
                      <*> availableThreshold
                      <*> balanceThreshold
                      <*> providerBaseUrl)



quiet :: Parser Bool
quiet = switch (short 'q'
               <> long "quiet"
               <> help "be quiet")


providerLogin :: Parser ProviderLogin
providerLogin = ProviderLogin
                <$> txtOption
                        ( long "user"
                        <> short 'u'
                        <> metavar "<USER>"
                        <> help "provider login user")
                <*> txtOption
                        ( long "pass"
                        <> short 'p'
                        <> metavar "<PASS>"
                        <> help "provider login password")


persistPath :: Parser (Maybe FilePath)
persistPath = optional $ strOption (  long "persist"
                                   <> metavar "<DIRECTORY>"
                                   <> help "directory path to persist values as csv")


endpoints :: Parser Endpoints
endpoints = fmap catMaybes $ pack <$> quota <*> used <*> available <*> balance <*> daysLeft
    where pack a b c d e = [a, b, c, d, e] -- FIXME: generic pack
          quota = optional $ EndpointQuota <$> txtOption
                  ( long "pub-quota"
                      <> metavar "<PUBLISH URL FOR QUOTA>"
                      <> help "endpoint for quota value")

          used = optional $ EndpointUsed <$> txtOption
               ( long "pub-used"
                 <> metavar "<PUBLISH URL FOR USED>"
                 <> help "endpoint for used value")

          available = optional $ EndpointAvailable <$> txtOption
                    ( long "pub-available"
                      <> metavar "<PUBLISH URL FOR AVAILABLE>"
                      <> help "endpoint for available value")

          balance = optional $ EndpointBalance <$> txtOption
                    ( long "pub-balance"
                    <> metavar "<PUBLISH URL FOR BALANCE>"
                    <> help "endpoint for current balance")

          daysLeft = optional $ EndpointDaysLeft <$> txtOption
                     ( long "pub-days-left"
                     <> metavar "<PUBLISH URL FOR PREPAID DAYS LEFT>"
                     <> help "endpoint for prepaid days left")



availableThreshold :: Parser AvailableThreshold
availableThreshold = AvailableThreshold
            <$> (optional $ option auto
                              ( long "available-warning"
                              <> help "available warning threshold"))
            <*> (optional $ option auto
                              ( long "available-critical"
                              <> help "available critical threshold"))



balanceThreshold :: Parser BalanceThreshold
balanceThreshold = BalanceThreshold
                   <$> (optional $ option auto
                                     ( long "balance-warning"
                                     <> help "balance warning threshold"))
                   <*> (optional $ option auto
                                     ( long "balance-critical"
                                      <> help "balance critical threshold"))


providerBaseUrl :: Parser ProviderBaseUrl
providerBaseUrl = txtOption (  long "provider-base-url"
                            <> metavar "PROVIDER_BASE_URL"
                            <> value "https://www.alditalk-kundenbetreuung.de"
                            <> help "Base URL from the Provider Website to query the current values")






-- | optparse-applicative builder for an option taking a 'Text' argument
--
--   obsolte when https://github.com/pcapriotti/optparse-applicative/pull/223 get merged
--
txtOption :: Mod OptionFields Text -> Parser Text
txtOption = option txt
    where txt = fmap T.pack readerAsk
