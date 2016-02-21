module PublishTariff where

import           Control.Exception
import           Control.Monad              (unless)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Lazy       as BL
import           Data.Time.Clock.POSIX
import qualified Network.Wreq               as W
import qualified Text.StringTemplate        as ST
import           Types


publishTariff :: Tariff -> ReaderT AppConfig IO ()
publishTariff t = do
  endpoints <- asks acPublishEndpoints
  quiet <- asks acQuiet
  ts <- lift $ show . round . (* 1000) <$> getPOSIXTime
  lift $ mapM_ (publish ts t quiet) endpoints


-- FIXME: better error handling
publish :: String -> Tariff -> Bool -> Endpoint -> IO ()
publish ts t quiet e = do
  maybe (pure ()) (post . enrich) $ resolve t e
  where enrich (url, value) = ST.render $ ST.setManyAttrib [("ts", ts), ("value", value)] $ ST.newSTMP url
        post url = do
          unless quiet $ putStr $ "Publish to: " ++ url
          res <- try $ W.post url BL.empty :: IO (Either SomeException (W.Response BL.ByteString))
          unless quiet $ putStrLn $ either ((++) " - ERROR: " . show) (const " - OK") res


resolve :: Tariff -> Endpoint -> Maybe (String, String)
resolve (Tariff b _) (EndpointBalance url) = Just (url, show b)
resolve (Tariff _ UsageNotAvailable) _ = Nothing
resolve (Tariff _ u) (EndpointQuota url) = Just (url, show $ uQuota u)
resolve (Tariff _ u) (EndpointUsed url) = Just (url, show $ uUsed u)
resolve (Tariff _ u) (EndpointAvailable url) = Just (url, show $ uAvailable u)
