{-# LANGUAGE ScopedTypeVariables #-}
module PublishTariff where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Lazy       as BL
import           Data.Time.Clock.POSIX
import qualified Network.Wreq               as W
import           Text.StringTemplate
import           Types


publishTariff :: Tariff -> App Tariff
publishTariff t@(Tariff _ UsageNotAvailable) = return t
publishTariff t@(Tariff balance usage) = do
  endpoints <- asks acPublishEndpoints
  _ <- lift . lift $ do
         ts <- show . round . (* 1000) <$> getPOSIXTime
         mapM_ (publish ts t) endpoints
  return t


-- FIXME: better error handling
publish :: String -> Tariff -> Endpoint -> IO ()
publish ts t e = do
  let url = enrich ts t e
  _ <- putStr $ "Publish to: " ++ url
  errOrRes <- try $ W.post url BL.empty
  case errOrRes of
    Left (e :: SomeException) -> putStrLn $ " - ERROR: " ++ show e
    Right res -> putStrLn " - OK"
  return ()



enrich :: String -> Tariff -> Endpoint -> String
enrich ts t e = let (url, value) = lookup e t
                in render $ setManyAttrib [("ts", ts), ("value", show value)] $ newSTMP url
    where lookup (EndpointQuota url) t = (url, uQuota . tUsage $ t)
          lookup (EndpointUsed url)  t = (url, uUsed . tUsage $ t)
          lookup (EndpointAvailable url) t = (url, uAvailable . tUsage $ t)
          lookup (EndpointBalance url) t = (url, round . tBalance $ t)
