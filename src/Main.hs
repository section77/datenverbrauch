{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Args
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Version               (showVersion)
import           Options.Applicative
import           Paths_datenverbrauch       (version)
import           QueryStats
import           System.Environment         (getArgs)
import           Types


main :: IO ()
main = execParser opts >>= run
    where opts = info (helper <*> appArgs)
                 (fullDesc
                 <> progDesc "query internet access data usage"
                 <> header "datenverbrauch")



run :: AppArgs -> IO ()
run ShowVersion = putStrLn ("version: " ++ showVersion version)
run (Run ac) = do
  errOrRes <- runExceptT $ (runReaderT queryStats ac)
  case errOrRes of
    Left e -> print e
    Right s -> putStrLn s


