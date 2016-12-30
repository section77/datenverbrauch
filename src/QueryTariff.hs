{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module QueryTariff where

import           BasicPrelude
import           Control.Lens
import           Control.Monad.Trans.Except (ExceptT (..), throwE)
import           Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Lazy       as LBS
import           Data.Char                  (isDigit)
import qualified Data.Text                  as T
import           Data.Time                  (Day, diffDays)
import           Data.Time.Clock            (getCurrentTime, utctDay)
import           Data.Time.Format           (defaultTimeLocale, parseTimeM)
import           Network.Wreq
import qualified Network.Wreq.Session       as WS
import           Text.HTML.TagSoup
import           Types


-- | query the actual tariff from the provider
--
queryTariff :: App Tariff
queryTariff = do
  baseUrl <- asks acProviderBaseUrl
  withSession $ \sess -> do
                res <- lift $ WS.get sess (baseUrl ++ "/de/tarif/mein-tarif")
                let body = res ^. responseBody
                today <- lift (utctDay <$> getCurrentTime)
                either throwE pure $ extract today (parseTags body)
    where extract :: Day -> [Tag LByteString] -> Either AppError Tariff
          extract today tags = do
            balance <- extractBalance tags
            usage   <- extractUsage tags
            end     <- extractEndDate tags
            pure $ Tariff balance usage (diffDays end today)




-- | extract the balance
--
extractBalance :: [Tag LByteString] -> Either AppError Balance
extractBalance tags = maybe (Left BalanceNotFound) extract $ maybeBalanceTag tags >>= maybeTagText
    where maybeBalanceTag = listToMaybe . drop 5 . dropWhile (~/== "<div id=ajaxReplaceQuickInfoBoxBalanceId>")
          extract = Right . readBalance . takeBalance . dropNonDigits . show
          dropNonDigits = T.dropWhile (not . isDigit)
          takeBalance = T.takeWhile (liftM2 (||) isDigit (== ','))
          readBalance = Balance . read . T.map (\c -> if(c == ',') then '.' else c)



-- | extract the usage
--
extractUsage :: [Tag LByteString] -> Either AppError Usage
extractUsage tags = maybe (Right UsageNotAvailable) extract $ maybeUsageTag tags >>= maybeTagText
  where maybeUsageTag = listToMaybe . drop 4 . dropWhile (~/== "Datenverbrauch")
        -- TODO: use regex to parse:
        -- "\n                                        Noch 5046 von 5120 MB verf\195\188gbar\n                                    "
        extract x = case (words . show) x of
                             [_, _, available, _, quota, _, _, _] -> let available' = read available
                                                                         quota' = read quota
                                                                     in Right $ Usage available' (quota' - available') quota'
                             _                                    -> Left UsageNotExtractable


-- | extract the prepaid end date
extractEndDate :: [Tag LByteString] -> Either AppError Day
extractEndDate tags = maybe (Left EndDateNotFound) parseDate $ maybeEndDateTag tags >>= maybeTagText
  where maybeEndDateTag = listToMaybe . drop 4 . dropWhile (~/== "Laufzeitende")
        parseDate (extract -> s) = maybe (Left $ EndDateNotParsable s) Right $ parseTimeM True defaultTimeLocale "%d.%m.%y" s
        extract = init . tail . textToString . show



-- | wrapper to handle the login in the background
--
withSession :: (WS.Session -> ExceptT AppError IO a) -> App a
withSession f = do
  baseUrl <- asks acProviderBaseUrl
  providerLogin <- asks acProviderLogin
  lift $ (ExceptT $ login baseUrl providerLogin) >>= f


-- | login in the provider web app
--
login :: ProviderBaseUrl -> ProviderLogin -> IO (Either AppError WS.Session)
login baseUrl pl = WS.withSession $ \sess -> do
             -- invalidate old session
             _ <- WS.post sess (baseUrl <> "/de/logout") LBS.empty
             -- go to the base url, and extract the actual login token
             token <- (\res -> res ^. responseBody . to parseTags . to extractToken) <$> WS.get sess baseUrl
             -- build login params
             let loginParams = ["_username" := plUser pl, "_password" := plPass pl, "_csrf_token" := token]
             -- perform login
             bimap (LoginError . show) (const sess) <$> (try (WS.post sess (baseUrl ++ "/de/login_check") loginParams) :: IO (Either SomeException (Response LByteString)))
                 where extractToken :: [Tag LByteString] -> Maybe LByteString
                       extractToken = fmap (fromAttrib "value") . listToMaybe . dropWhile (~/== "<input name=_csrf_token")



-- Helper to fix the selector Type to String.
-- TagSoup has two Type Classes for the selector: String and (TagRep (Tag str)).
-- With the use of 'OverloadedString', we get ambiguous Type Classes for the selector.
(~/==) a b = a ~/= (b :: String)
(~===) a b = a ~== (b :: String)
