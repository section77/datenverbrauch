{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Exception.Base (displayException)
import qualified Data.Text              as T
import           Data.Time
import           Data.Version           (showVersion)
import           Formatting
import           Options.Applicative
import           Paths_datenverbrauch   (version)
import           Protolude              hiding ((%))
import           Text.Printf

import           Args
import           Persist
import           PublishTariff
import           QueryTariff
import           Types

main :: IO ()
main = execParser opts >>= run
  where opts = info (helper <*> appArgs)
               ( fullDesc
               <> progDesc (toS $ T.unlines
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
-- >>> :set -XOverloadedStrings
-- >>> let t = Tariff (Balance 10) (Usage 500 230 270) 21
-- >>> let at = AvailableThreshold Nothing Nothing
-- >>> let bt = BalanceThreshold Nothing Nothing
-- >>> let cfg = AppConfig False (ProviderLogin "" "") Nothing [] at bt "http://provider.url.com"
-- >>> runReaderT (evalRes (Right t)) cfg
-- --------------------
-- Balance:    10.00 €
-- --------------------
-- Quota:       500 MB
-- Used:        230 MB
-- Available:   270 MB
-- --------------------
-- Days left:       21
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
-- Balance:    10.00 €
-- --------------------
-- Quota:       500 MB
-- Used:        230 MB
-- Available:   270 MB
-- --------------------
-- Days left:       21
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
     let sep = T.replicate 20 "-"
     logger $ unlines' [
                  sep
                , balanceReport balance
                , sep
                , usageReport usage
                , sep
                , sformat ("Days left: " % (left 8 ' ' %. int)) daysLeft
                , sep
                ]



     --
     -- persist the values when a path is given
     --
     maybePersistPath <- asks acPersistPath
     let persistAndLogResult path = do
           res <- persist tariff path
           case res of
             (Right n) -> unless beQuiet . putText $ "values persisted in file: " <> T.pack n
             (Left e)  -> putStrLn $ "unable to persist values: " <> displayException e
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
  lift $ do putText $ "ERROR: " <> show e
            exitWith $ ExitFailure 2



-- | build the report for the balance
--
balanceReport :: Balance -> Text
balanceReport (Balance b) = sformat ("Balance:" % (left 9 ' ' %. fixed 2) % " €") b
balanceReport _           = "Balance not available\n"



-- | build the report for the usage
--
usageReport :: Usage -> Text
usageReport (Usage q u a) = sformat ("Quota:     " % (left 5 ' ' %. int) % " MB" %
                                   "\nUsed:      " % (left 5 ' ' %. int) % " MB" %
                                   "\nAvailable: " % (left 5 ' ' %. int) % " MB" ) q u a
usageReport _             = "Usage not available - quota exhausted?"




-- | log to stdout unless '--quite' flag was passed
--
logger :: Text -> ReaderT AppConfig IO ()
logger txt = do
  quiet <- asks acQuiet
  unless quiet . lift . putText $ txt



-- | like unlines from the prelude, but without the last newline
--
unlines' :: [Text] -> Text
unlines' = T.init . T.unlines
