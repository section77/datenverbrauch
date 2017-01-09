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
-- we use the standard prelude here and go with strings because
-- optparse-applicative and printf expects strings
import           BasicPrelude               (liftIO)
import           Persist
import           Prelude
import           PublishTariff
import           QueryTariff
import           System.Exit                (ExitCode (..), exitWith)
import           Text.Printf
import           Types



main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> appArgs)
               ( fullDesc
               <> progDesc (unlines
                            ["run it to show the current usage, use '--pub-xxx' switch to publish the values."
                            ,"use $ts$ for the current timestamp in ms epoch and $value$ for the current value in the url."
                            ])
               <> header "datenverbrauch - query internet access data usage")




run :: AppArgs -> IO ()
run ShowVersion = printf "version: %s\n" (showVersion version)
run (Run ac) = do
  unless (acQuiet ac) printHeader
  res <- runExceptT $ runReaderT queryTariff ac
  runReaderT (evalRes res) ac
      where printHeader = do
                 tz <- getCurrentTimeZone
                 ts <- utcToLocalTime tz <$> getCurrentTime
                 let date = formatTime defaultTimeLocale "%d.%m.%0Y %R" ts
                 printf "Startup - date: %s - version: %s\n" date (showVersion version)





-- | eval the run result
--
-- >>> -- all fine
-- >>> let t = Tariff (Balance 10) (Usage 500 230 270) 21
-- >>> let at = AvailableThreshold Nothing Nothing
-- >>> let bt = BalanceThreshold Nothing Nothing
-- >>> let cfg = AppConfig False (ProviderLogin "" "") Nothing [] at bt "http://provider.url.com"
-- >>> runReaderT (evalRes (Right t)) cfg
-- --------------------
-- Balance:    10.0 €
-- --------------------
-- Quota:      500 MB
-- Used:       230 MB
-- Available:  270 MB
-- --------------------
-- Days left:      21
-- --------------------
--
--
-- >>> -- available warning threshold
-- >>> let t = Tariff (Balance 10) (Usage 500 230 270) 21
-- >>> let at = AvailableThreshold (Just 280) Nothing
-- >>> let bt = BalanceThreshold Nothing Nothing
-- >>> let cfg = AppConfig False (ProviderLogin "" "") Nothing [] at bt "http://provider.url.com"
-- >>> runReaderT (evalRes (Right t)) cfg
-- --------------------
-- Balance:    10.0 €
-- --------------------
-- Quota:      500 MB
-- Used:       230 MB
-- Available:  270 MB
-- --------------------
-- Days left:      21
-- --------------------
-- available below warning threshold!
-- *** Exception: ExitFailure 1
evalRes :: Either AppError Tariff -> ReaderT AppConfig IO ()
-- handle successful result
evalRes (Right tariff@(Tariff balance usage daysLeft)) = do
     beQuiet <- asks acQuiet

     --
     -- build / print the report
     --
     let sep = replicate 20 '-'
     logger $ unlines' [
                  sep
                , balanceReport balance
                , sep
                , usageReport usage
                , sep
                , printf "Days left: %7d" daysLeft
                , sep
                ]



     --
     -- persist the values when a path is given
     --
     maybePersistPath <- asks acPersistPath
     let persistAndLogResult path = do
           res <- persist tariff path
           case res of
             (Right n) -> unless beQuiet . putStrLn $ "values persisted in file: " <> n
             (Left e)  -> putStrLn $ "unable to persist values: " <> show e
     liftIO $ mapM_ persistAndLogResult maybePersistPath





     --
     -- publish the values
     --
     --  * when no usage is available, publish zeros
     --    and terminate with exit code 2
     --
     case usage of
       UsageNotAvailable -> do
                  logger "publish zeros for usage"
                  publishTariff (tariff { tUsage = Usage 0 0 0 })
                  lift $ exitWith (ExitFailure 2)

       _                 -> publishTariff tariff



     --
     -- log warnings / terminate with failure exit code if values are below the threshold
     --
     availableBelowCritial <- isBelowCritical usage
     when availableBelowCritial $ do
       logger "available below critial threshold!"
       lift $ exitWith (ExitFailure 2)

     availableBelowWarning <- isBelowWarning usage
     when availableBelowWarning $ do
       logger "available below warning threshold!"
       lift $ exitWith (ExitFailure 1)

     balanceBelowWarning <- isBelowCritical balance
     when balanceBelowWarning $ do
       logger "balance below critial threshold!"
       lift $ exitWith (ExitFailure 2)

     balanceBelowNotification <- isBelowWarning balance
     when balanceBelowNotification $ do
       logger "balance below warning threshold!"
       lift $ exitWith (ExitFailure 1)

-- handle errors
evalRes (Left e) =
  lift $ do putStrLn $ "ERROR: " ++ show e
            exitWith $ ExitFailure 2



-- | build the report for the balance
--
balanceReport :: Balance -> String
balanceReport (Balance b) = printf "Balance: %7f €" b
balanceReport _           = "Balance not available\n"



-- | build the report for the usage
--
usageReport :: Usage -> String
usageReport (Usage q u a) =  unlines' [
                              printf "Quota:     %4d MB" q
                             ,printf "Used:      %4d MB" u
                             ,printf "Available: %4d MB" a ]
usageReport _             = "Usage not available - quota exhausted?"




-- | log to stdout unless '--quite' flag was passed
--
logger :: String -> ReaderT AppConfig IO ()
logger msg = do
  quiet <- asks acQuiet
  unless quiet $ lift . putStrLn $ msg



-- | like unlines from the prelude, but without the last newline
--
unlines' :: [String] -> String
unlines' = init . unlines
