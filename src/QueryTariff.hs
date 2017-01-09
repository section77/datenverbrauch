{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module QueryTariff where

import           BasicPrelude
import           Control.Lens
import           Control.Monad.Trans.Except (ExceptT (..), throwE)
import           Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.Char                  (isDigit)
import qualified Data.Text                  as T
import           Data.Time                  (Day, diffDays)
import           Data.Time.Clock            (getCurrentTime, utctDay)
import           Data.Time.Format           (defaultTimeLocale, parseTimeM)
import           Network.Wreq
import qualified Network.Wreq.Session       as WS
import           Network.Wreq.Types         (Postable)
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
          extract today tags = Tariff
                               <$> extractBalance tags
                               <*> extractUsage tags
                               <*> extractDaysLeft today tags



-- | extract the balance
--
--   <div id="ajaxReplaceQuickInfoBoxBalanceId">
--     <strong>
--       <p>20,13&nbsp;€</p>
--     </strong>
--   </div>
--
extractBalance :: [Tag LByteString] -> Either AppError Balance
extractBalance tags = maybe (Left BalanceNotFound) extract $ maybeBalanceTag tags >>= maybeTagText
    where maybeBalanceTag = listToMaybe . drop 5 . dropWhile (~/== "<div id=ajaxReplaceQuickInfoBoxBalanceId>")
          extract = Right . readBalance . takeBalance . decodeUtf8 . BSL.toStrict
          takeBalance = T.takeWhile (liftM2 (||) isDigit (== ','))
          readBalance = Balance . read . T.map (\c -> if(c == ',') then '.' else c)



-- | extract the usage
--
--   <tr>
--     <td>Datenverbrauch</td>
--     <td>
--          Noch 5046 von 5120 MB verfügbar
--     </td>
--   </tr>
--
extractUsage :: [Tag LByteString] -> Either AppError Usage
extractUsage tags = maybe (Right UsageNotAvailable) extract $ maybeUsageTag tags >>= maybeTagText
  where maybeUsageTag = listToMaybe . drop 4 . dropWhile (~/== "Datenverbrauch")
        -- TODO: use regex to parse:
        -- "\n                                        Noch 5046 von 5120 MB verf\195\188gbar\n                                    "
        extract x = case (words . show) x of
                             [_, _, available, _, quota, _, _, _] -> let available' = read available
                                                                         quota' = read quota
                                                                     in Right $ Usage quota' (quota' - available') available'
                             _                                    -> Left UsageNotExtractable



-- | extract how many days left
--
--   <tr class="t-row">
--     <td class="t-label">Laufzeitende</td>
--     <td>27.01.17</td>
--     <td>&nbsp;</td>
--   </tr>
--
extractDaysLeft :: Day -> [Tag LByteString] -> Either AppError Integer
extractDaysLeft today tags = maybe (Left EndDateNotFound) extractDaysLeft $ maybeEndDateTag tags >>= maybeTagText
  where maybeEndDateTag = listToMaybe . drop 4 . dropWhile (~/== "Laufzeitende")
        extractDaysLeft (BSL.unpack -> s) = bimap EndDateNotParsable calcDaysLeft $ parseTimeM True defaultTimeLocale "%d.%m.%y" s
        calcDaysLeft end = diffDays end today



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
             _ <- WS.post sess (baseUrl <> "/de/logout") BSL.empty
             -- go to the base url, and extract the actual login token
             token <- extractToken . parseTags . view responseBody <$> WS.get sess baseUrl
             -- build login params
             let loginParams = ["_username" := plUser pl, "_password" := plPass pl, "_csrf_token" := token]
             -- perform login
             bimap (LoginError . show) (const sess) <$> tryPost sess (baseUrl <> "/de/login_check") loginParams
                 where tryPost :: Postable a => WS.Session -> String -> a -> IO (Either SomeException (Response LByteString))
                       tryPost sess url payload = try $ WS.post sess url payload
                       extractToken :: [Tag LByteString] -> Maybe LByteString
                       extractToken = fmap (fromAttrib "value") . listToMaybe . dropWhile (~/== "<input name=_csrf_token")



-- Helper to fix the selector Type to String.
-- TagSoup has two Type Classes for the selector: String and (TagRep (Tag str)).
-- With the use of 'OverloadedString', we get ambiguous Type Classes for the selector.
(~/==) a b = a ~/= (b :: String)
(~===) a b = a ~== (b :: String)
