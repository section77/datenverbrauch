module Args where

import           Data.Maybe          (catMaybes)
import           Options.Applicative
import           Types

appArgs :: Parser AppArgs
appArgs = flag' ShowVersion (short 'v' <> long "version" <> help "app version")
          <|> Run <$> (AppConfig <$> providerLogin <*> endpoints <*> usageThreshold)


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
endpoints = fmap catMaybes $ pack <$> quota <*> used <*> available
    where pack a b c = [a, b, c] -- FIXME: generic pack
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



usageThreshold :: Parser UsageThreshold
usageThreshold = (UsageThreshold
            <$> option auto
                ( long "usage-notification"
                <> help "usage notification threshold")
            <*> option auto
                ( long "usage-warning"
                <> help "usage warning threshold")
             ) <|> pure WithoutUsageThreshold


