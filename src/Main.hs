{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Args
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
run (Run ac) = printHeader >> runExceptT (runReaderT (queryTariff >>= publishTariff) ac) >>= evalRun (acUsageThreshold ac)
  where printHeader = do
          tz <- getCurrentTimeZone
          t <- utcToLocalTime tz <$> getCurrentTime
          printf "Startup - date: %s\n" $ formatTime defaultTimeLocale "%d.%m.%0Y %R" t



evalRun :: UsageThreshold -> Either AppError Tariff -> IO ()
evalRun ut l@(Left e) = putStrLn ("ERROR: " ++ show e) >> exit ut l
evalRun ut r@(Right tariff) = printTariff tariff >> exit ut r



exit :: UsageThreshold -> Either AppError Tariff -> IO ()
exit _ (Left e) = exitWith $ ExitFailure 2
exit _ (Right (Tariff _ UsageNotAvailable)) = exitWith $ ExitFailure 2
exit WithoutUsageThreshold _ = exitWith ExitSuccess
exit (UsageThreshold n w) (Right (Tariff _ (Usage _ _ a))) = if a < w then exitWith $ ExitFailure 2
                                                             else if a < n then exitWith $ ExitFailure 1
                                                             else exitWith ExitSuccess


printTariff :: Tariff -> IO ()
printTariff (Tariff balance usage) = do
  printf("------------------\n")
  printBalance balance
  printUsage usage
    where printBalance = printf "Balance:   %f €\n"
          printUsage UsageNotAvailable = putStrLn "Usage not available - quota exhausted?"
          printUsage (Usage q u a) = do
            printf "Quota:     %d MB\n" q
            printf "Used:      %d MB\n" u
            printf "Available: %d MB\n" a
