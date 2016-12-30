{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Args
import           Control.Monad              (unless, when)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Time
import           Data.Version               (showVersion)
import           Options.Applicative
import           Paths_datenverbrauch       (version)
import           Prelude                    hiding (log)
import           PublishTariff
import           QueryTariff
import           System.Exit                (ExitCode (..), exitWith)
import           Text.Printf
import           Types

main :: IO ()
main = execParser opts >>= run
    where opts = info (helper <*> appArgs)
                 (fullDesc
                 <> progDesc (unlines [ "run it to show the current usage, use '--pub-xxx' switch to publish the values."
                                      , "use $ts$ for the current timestamp in ms epoch and $value$ for the current value in the url"
                                      ])
                 <> header "datenverbrauch - query internet access data usage")



run :: AppArgs -> IO ()
run ShowVersion = putStrLn ("version: " ++ showVersion version)
run (Run ac) = do
  unless (acQuiet ac) $ printHeader
  res <- runExceptT $ runReaderT queryTariff ac
  runReaderT (evalRes res) ac
    where printHeader = do
            tz <- getCurrentTimeZone
            t <- utcToLocalTime tz <$> getCurrentTime
            printf "Startup - date: %s\n" $ formatTime defaultTimeLocale "%d.%m.%0Y %R" t




-- | eval the run result
--
-- >>> -- all fine
-- >>> let t = Tariff (Balance 10) (Usage 500 230 270) 21
-- >>> let at = AvailableThreshold Nothing Nothing
-- >>> let bt = BalanceThreshold Nothing Nothing
-- >>> let cfg = AppConfig False (ProviderLogin "" "") [] at bt "http://provider.url.com"
-- >>> runReaderT (evalRes (Right t)) cfg
-- ------------------
-- Balance:   10.0 €
-- ------------------
-- Quota:     500 MB
-- Used:      230 MB
-- Available: 270 MB
-- ------------------
-- Days left: 21
--
--
-- >>> -- available warning threshold
-- >>> let t = Tariff (Balance 10) (Usage 500 230 270) 21
-- >>> let at = AvailableThreshold (Just 280) Nothing
-- >>> let bt = BalanceThreshold Nothing Nothing
-- >>> let cfg = AppConfig False (ProviderLogin "" "") [] at bt "http://provider.url.com"
-- >>> runReaderT (evalRes (Right t)) cfg
-- ------------------
-- Balance:   10.0 €
-- ------------------
-- Quota:     500 MB
-- Used:      230 MB
-- Available: 270 MB
-- ------------------
-- Days left: 21
-- available below warning threshold!
-- *** Exception: ExitFailure 1
evalRes :: Either AppError Tariff -> ReaderT AppConfig IO ()
-- handle successful result
evalRes (Right t@(Tariff b u l)) = do
  availableBelowCritial <- isBelowCritical u
  availableBelowWarning <- isBelowWarning u
  balanceBelowWarning <- isBelowCritical b
  balanceBelowNotification <- isBelowWarning b
  case b of
    (Balance bv) -> log $ concat [
                     "------------------\n"
                    , printf "Balance:   %f €\n" bv
                    , "------------------"
                    ]
    BalanceNotAvailable -> log "Balance not available\n"
  if isUsageAvailable u then
      do log $ concat [
                   printf "Quota:     %d MB\n" (uQuota u)
                 , printf "Used:      %d MB\n" (uUsed u)
                 , printf "Available: %d MB\n" (uAvailable u)
                 , printf "------------------"
                 ]
         publishTariff t
  else
      do log $ concat [
                  "Usage not available - quota exhausted? (publish zeros for usage)\n"
                 , "------------------"
                 ]
         publishTariff (t { tUsage = Usage 0 0 0 })
         lift $ exitWith (ExitFailure 2)
  log $ printf "Days left: %d" l

  when availableBelowCritial $ do
    log "available below critial threshold!"
    lift $ exitWith (ExitFailure 2)

  when availableBelowCritial $ do
    log "available below critial threshold!"
    lift $ exitWith (ExitFailure 2)

  when availableBelowWarning $ do
    log "available below warning threshold!"
    lift $ exitWith (ExitFailure 1)

  when balanceBelowWarning $ do
    log "balance below critial threshold!"
    lift $ exitWith (ExitFailure 2)

  when balanceBelowNotification $ do
    log "balance below warning threshold!"
    lift $ exitWith (ExitFailure 1)

-- handle errors
evalRes (Left e) = lift $ do
                  putStrLn $ "ERROR: " ++ show e
                  exitWith $ ExitFailure 2





isUsageAvailable :: Usage -> Bool
isUsageAvailable UsageNotAvailable = False
isUsageAvailable _ = True


log :: String -> ReaderT AppConfig IO ()
log msg = do
  quiet <- asks acQuiet
  unless quiet $ lift . putStrLn $ msg
