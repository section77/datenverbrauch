module Args (appArgs)  where

import           BasicPrelude
import           Options.Applicative
import           Types

appArgs :: Parser AppArgs
appArgs = flag' ShowVersion (short 'v' <> long "version" <> help "app version")
          <|> Run <$> (AppConfig <$> quiet <*> providerLogin <*> endpoints <*> availableThreshold <*> balanceThreshold <*> providerBaseUrl)

quiet :: Parser Bool
quiet = switch (short 'q'
               <> long "quiet"
               <> help "be quiet")

providerLogin :: Parser ProviderLogin
providerLogin = ProviderLogin
                <$> strOption
                        ( long "user"
                        <> short 'u'
                        <> metavar "<USER>"
                        <> help "provider login user")
                <*> strOption
                        ( long "pass"
                        <> short 'p'
                        <> metavar "<PASS>"
                        <> help "provider login password")


endpoints :: Parser Endpoints
endpoints = fmap catMaybes $ pack <$> quota <*> used <*> available <*> balance <*> daysLeft
    where pack a b c d e = [a, b, c, d, e] -- FIXME: generic pack
          quota = optional $ EndpointQuota <$> strOption
                  ( long "pub-quota"
                      <> metavar "<PUBLISH URL FOR QUOTA>"
                      <> help "endpoint for quota value")

          used = optional $ EndpointUsed <$> strOption
               ( long "pub-used"
                 <> metavar "<PUBLISH URL FOR USED>"
                 <> help "endpoint for used value")

          available = optional $ EndpointAvailable <$> strOption
                    ( long "pub-available"
                      <> metavar "<PUBLISH URL FOR AVAILABLE>"
                      <> help "endpoint for available value")

          balance = optional $ EndpointBalance <$> strOption
                    ( long "pub-balance"
                    <> metavar "<PUBLISH URL FOR BALANCE>"
                    <> help "endpoint for current balance")

          daysLeft = optional $ EndpointDaysLeft <$> strOption
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
providerBaseUrl = strOption (  long "provider-base-url"
                            <> metavar "PROVIDER_BASE_URL"
                            <> value "https://www.alditalk-kundenbetreuung.de"
                            <> help "Base URL from the Provider Website to query the current values")

