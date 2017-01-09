{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module PublishTariff where

import           BasicPrelude
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy       as BSL
import           Data.Time.Clock.POSIX
import qualified Network.Wreq               as W
import qualified Text.StringTemplate        as ST
import           Types


publishTariff :: Tariff -> ReaderT AppConfig IO ()
publishTariff t = do
  endpoints <- asks acPublishEndpoints
  quiet <- asks acQuiet
  ts <- lift $ textToString . show . round . (* 1000) <$> getPOSIXTime
  lift $ mapM_ (publish ts t quiet) endpoints


-- FIXME: better error handling
publish :: String -> Tariff -> Bool -> Endpoint -> IO ()
publish ts t quiet e = do
  maybe (pure ()) (post . enrich) $ resolve t e
  where enrich (url, value) = ST.render $ ST.setManyAttrib [("ts", ts), ("value", textToString value)] $ ST.newSTMP url
        post url = do
          unless quiet $ putStr $ "Publish to: " ++ url
          res <- try $ W.post (textToString url) BSL.empty :: IO (Either SomeException (W.Response LByteString))
          unless quiet $ putStrLn $ either ((++) " - ERROR: " . show) (const " - OK") res


resolve :: Tariff -> Endpoint -> Maybe (String, Text)
resolve (Tariff (Balance b) _ _) (EndpointBalance url)       = Just (url, show b)
resolve (Tariff BalanceNotAvailable _ _) (EndpointBalance _) = Nothing
resolve (Tariff _ UsageNotAvailable _) _                     = Nothing
resolve (Tariff _ u _) (EndpointQuota url)                   = Just (url, show $ uQuota u)
resolve (Tariff _ u _) (EndpointUsed url)                    = Just (url, show $ uUsed u)
resolve (Tariff _ u _) (EndpointAvailable url)               = Just (url, show $ uAvailable u)
resolve (Tariff _ _ dl) (EndpointDaysLeft url)               = Just (url, show dl)
