{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module PublishTariff where

import           Control.Exception.Base  (displayException)
import qualified Data.ByteString.Lazy    as BSL
import           Data.Time.Clock.POSIX
import qualified Network.Wreq.StringLess as W
import           Protolude
import qualified Text.StringTemplate     as ST
import           Types


publishTariff :: Tariff -> ReaderT AppConfig IO ()
publishTariff t = do
  endpoints <- asks acPublishEndpoints
  quiet <- asks acQuiet
  lift $ do
    ts <- round . (* 1000) <$> getPOSIXTime :: IO Integer
    mapM_ (publish (show ts) t quiet) endpoints


-- FIXME: better error handling
publish :: Text -> Tariff -> Bool -> Endpoint -> IO ()
publish ts t quiet e = do
  maybe (pure ()) (post . enrich) $ resolve t e
  where enrich :: (Text, Text) -> Text
        enrich (toS -> url, value) = ST.render $ ST.setManyAttrib [("ts", ts), ("value", show value)] $ ST.newSTMP url
        post url = do
          unless quiet $ putStr $ "Publish to: " <> url
          res <- try $ W.post url BSL.empty :: IO (Either SomeException (W.Response LByteString))
          unless quiet . putStrLn $ either ((<>) " - ERROR: " . displayException) (const " - OK") res


resolve :: Tariff -> Endpoint -> Maybe (Text, Text)
resolve (Tariff (Balance b) _ _) (EndpointBalance url)       = Just (url, show b)
resolve (Tariff BalanceNotAvailable _ _) (EndpointBalance _) = Nothing
resolve (Tariff _ UsageNotAvailable _) _                     = Nothing
resolve (Tariff _ u _) (EndpointQuota url)                   = Just (url, show $ uQuota u)
resolve (Tariff _ u _) (EndpointUsed url)                    = Just (url, show $ uUsed u)
resolve (Tariff _ u _) (EndpointAvailable url)               = Just (url, show $ uAvailable u)
resolve (Tariff _ _ dl) (EndpointDaysLeft url)               = Just (url, show dl)
