{-# LANGUAGE OverloadedStrings #-}
module Persist where


import           BasicPrelude
import qualified Data.Text.Lazy.IO          as TL
import           Data.Time
import           Formatting
import           Formatting.ShortFormatters (d, f, t)
import           Formatting.Time            (datetime)
import           System.Directory           (createDirectoryIfMissing,
                                             doesFileExist)
import           Types


type Header = ByteString
type Line = ByteString

persist :: Tariff -> FilePath -> IO (Either SomeException FilePath)
persist tariff dir = do
  createDirectoryIfMissing True dir
  today <- liftIO getCurrentTime
  let fileName = formatTime defaultTimeLocale "%Y_%m.csv" today
      path = dir </> fileName
      csv = toCSV today tariff

  try $ do
    -- write csv header if the file doen't exists
    fileExists <- doesFileExist path
    unless fileExists $ TL.appendFile path csvHeader

    -- write actual values
    TL.appendFile path csv

    return path




-- | CSV header line
--
csvHeader :: LText
csvHeader = "timestamp,balance,quota,used,available,days-left\n"


-- | CSV line
--
-- >>> let tariff = Tariff (Balance 9.95) (Usage 400 210 190) 13
-- >>> let today = Prelude.read "2017-01-09 18:01:33.742398 UTC" :: UTCTime
-- >>> toCSV today tariff
-- "Mon Jan  9 18:01:33 UTC 2017,9.95,400,210,190,13\n"
toCSV :: UTCTime -> Tariff -> LText
toCSV today (Tariff b u dl) = format fmt today (balance b) (usage u) dl
    where fmt = datetime % "," % f 2 % "," % t % "," % d % "\n"
          balance (Balance b')          = b'
          balance (BalanceNotAvailable) = 0
          usage (Usage q u' a)     = format (d % "," % d % "," % d) q u' a
          usage UsageNotAvailable  = "0,0,0"
