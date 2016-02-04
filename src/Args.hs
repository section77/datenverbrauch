module Args where

import           Data.Maybe          (catMaybes)
import           Options.Applicative
import           Types

appArgs :: Parser AppArgs
appArgs = flag' ShowVersion (short 'v' <> long "version" <> help "app version")
          <|> Run <$> (AppConfig <$> providerLogin <*> endpoints <*> availableThreshold <*> balanceThreshold)


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
endpoints = fmap catMaybes $ pack <$> quota <*> used <*> available <*> balance
    where pack a b c d = [a, b, c, d] -- FIXME: generic pack
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



availableThreshold :: Parser AvailableThreshold
availableThreshold = AvailableThreshold
            <$> (optional $ option auto
                              ( long "available-notification"
                              <> help "available notification threshold"))
            <*> (optional $ option auto
                              ( long "available-warning"
                              <> help "available warning threshold"))



balanceThreshold :: Parser BalanceThreshold
balanceThreshold = BalanceThreshold
                   <$> (optional $ option auto
                                     ( long "balance-notification"
                                     <> help "balance notification threshold"))
                   <*> (optional $ option auto
                                     ( long "balance-warning"
                                      <> help "balance warning threshold"))
