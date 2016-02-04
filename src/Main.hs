{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Args
import           Control.Monad              (when)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Time
import           Data.Version               (showVersion)
import           Options.Applicative
import           Paths_datenverbrauch       (version)
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
                                      , "use $ts$ for the current timestamp in ms epoch and $value$ for the current value in the url."
                                      ])
                 <> header "datenverbrauch - query internet access data usage")



run :: AppArgs -> IO ()
run ShowVersion = putStrLn ("version: " ++ showVersion version)
run (Run ac) = do
  printHeader
  res <- runExceptT $ runReaderT queryTariff ac
  runReaderT (evalRes res) ac
    where printHeader = do
            tz <- getCurrentTimeZone
            t <- utcToLocalTime tz <$> getCurrentTime
            printf "Startup - date: %s\n" $ formatTime defaultTimeLocale "%d.%m.%0Y %R" t




-- | eval the run result
--
-- >>> -- all fine
-- >>> let t = Tariff 10 $ Usage 500 230 270
-- >>> let at = AvailableThreshold Nothing Nothing
-- >>> let bt = BalanceThreshold Nothing Nothing
-- >>> runReaderT (evalRes (Right t)) $ AppConfig (ProviderLogin "" "") [] at bt
-- ------------------
-- Balance:   10.0 €
-- ------------------
-- Quota:     500 MB
-- Used:      230 MB
-- Available: 270 MB
-- ------------------
--
--
-- >>> -- available notification threshold
-- >>> let t = Tariff 10 $ Usage 500 230 270
-- >>> let at = AvailableThreshold (Just 280) Nothing
-- >>> let bt = BalanceThreshold Nothing Nothing
-- >>> runReaderT (evalRes (Right t)) $ AppConfig (ProviderLogin "" "") [] at bt
-- ------------------
-- Balance:   10.0 €
-- ------------------
-- Quota:     500 MB
-- Used:      230 MB
-- Available: 270 MB
-- ------------------
-- available below notification threshold!
-- *** Exception: ExitFailure 1
evalRes :: Either AppError Tariff -> ReaderT AppConfig IO ()
-- handle successful result
evalRes (Right t@(Tariff b u)) = do
  endpoints <- asks acPublishEndpoints
  availableBelowWarning <- isBelowWarning u
  availableBelowNotification <- isBelowNotification u
  balanceBelowWarning <- isBelowWarning b
  balanceBelowNotification <- isBelowNotification b
  lift $ do
    printf "------------------\n"
    printf "Balance:   %f €\n" b
    printf "------------------\n"
    if isUsageAvailable u then do
            printf "Quota:     %d MB\n" $ uQuota u
            printf "Used:      %d MB\n" $ uUsed u
            printf "Available: %d MB\n" $ uAvailable u
            printf "------------------\n"
            publishTariff endpoints t
    else do
           putStrLn "Usage not available - quota exhausted? (publish zeros)"
           putStrLn "------------------"
           publishTariff endpoints $ t { tUsage = Usage 0 0 0 }
           exitWith (ExitFailure 2)
    when availableBelowWarning $ putStrLn "available below warning threshold!" >> exitWith (ExitFailure 2)
    when availableBelowNotification $ putStrLn "available below notification threshold!" >> exitWith (ExitFailure 1)
    when balanceBelowWarning $ putStrLn "balance below warning threshold!" >> exitWith (ExitFailure 2)
    when balanceBelowNotification $ putStrLn "balance below notification threshold!" >> exitWith (ExitFailure 1)
-- handle errors
evalRes (Left e) = lift $ do
                  putStrLn $ "ERROR: " ++ show e
                  exitWith $ ExitFailure 2





isUsageAvailable :: Usage -> Bool
isUsageAvailable UsageNotAvailable = False
isUsageAvailable _ = True
