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
publishTariff tariff@(Tariff balance usage) = do
  endpoints <- asks acPublishEndpoints
  _ <- lift . lift $ do
         ts <- show . round . (* 1000) <$> getPOSIXTime
         mapM_ (publish ts usage) endpoints
  return tariff


-- FIXME: better error handling
publish :: String -> Usage -> Endpoint -> IO ()
publish ts u e = do
  let url = enrich ts u e
  _ <- putStr $ "Publish to: " ++ url
  errOrRes <- try $ W.post url BL.empty
  case errOrRes of
    Left (e :: SomeException) -> putStrLn $ " - ERROR: " ++ show e
    Right res -> putStrLn " - OK"
  return ()



enrich :: String -> Usage -> Endpoint -> String
enrich ts u e = let (url, value) = lookup e u
                in render $ setManyAttrib [("ts", ts), ("value", show value)] $ newSTMP url
    where lookup (EndpointQuota url) u = (url, uQuota u)
          lookup (EndpointUsed url)  u = (url, uUsed u)
          lookup (EndpointAvailable url) u = (url, uAvailable u)
