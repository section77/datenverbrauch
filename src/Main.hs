{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Args
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Time
import           Data.Version               (showVersion)
import           Options.Applicative
import           Paths_datenverbrauch       (version)
import           PublishUsage
import           QueryUsage
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
run (Run ac) = printHeader >> runExceptT (runReaderT (queryUsage >>= publishUsage) ac) >>= printResult
  where printHeader = do
          t <- getCurrentTime
          printf "Startup - date: %s\n" $ formatTime defaultTimeLocale "%d.%m.%0Y %R" t



printResult :: Either AppError Usage -> IO ()
printResult (Right usage) = do
  printf("------------------\n")
  printf "Quota:     %d MB\n" $ uQuota usage
  printf "Used:      %d MB\n" $ uUsed usage
  printf "Available: %d MB\n" $ uAvailable usage
printResult (Left e) = putStrLn $ "ERROR: " ++ show e

