{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module QueryTariff where

import           Control.Lens
import           Control.Monad.Trans.Except      (ExceptT (..), throwE)
import qualified Data.ByteString.Lazy.Char8      as BSL
import           Data.Char                       (isDigit)
import qualified Data.Text                       as T
import           Data.Text.Read
import           Data.Time                       (Day, diffDays)
import           Data.Time.Clock                 (getCurrentTime, utctDay)
import           Data.Time.Format                (defaultTimeLocale, parseTimeM)
import           Network.Wreq.StringLess
import qualified Network.Wreq.StringLess.Session as WS
import           Network.Wreq.StringLess.Types   (Postable)
import           Protolude
import           Text.HTML.TagSoup
import           Text.StringLike

import           Types


-- | query the actual tariff from the provider
--
queryTariff :: App Tariff
queryTariff = do
  baseUrl <- asks acProviderBaseUrl
  withSession $ \sess -> do
                res <- lift $ WS.get sess (baseUrl <> "/de/tarif/mein-tarif")
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
extractBalance tags = firstTagTextBy "Balance" locator tags >>= extract
    where locator = drop 5 . dropWhile (~/== "<div id=ajaxReplaceQuickInfoBoxBalanceId>")
          extract = readBalance . takeBalance . toS
          takeBalance = T.takeWhile (liftM2 (||) isDigit (== ','))
          tr f t = T.map (\c -> if(c == f) then t else c)
          readBalance = bimap BalanceNotParsable Balance . rational' . (tr ',' '.')



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
extractUsage tags = extract $ locator tags
    where locator = fmap (toS . fromTagText) . filter isTagText . take 9 . drop 15 . dropWhile (~/== "Internet-Flatrate XL")

          -- this is our input:
          --
          --   ["\n                                        Noch 2761 verf\195\188gbar\n                                    "
          --   ,"\n                                \n                                \n                                                    \n                                                    "
          --   ,"\n                                \n                                                                    "
          --   ,"von 5120 MB"]
          extract [T.words -> (_:a:_), _, _, T.words -> (_:q:_)] = buildUsage q a
          extract _                                              = Left UsageNotExtractable

          buildUsage q a = let errOrQuota     = decimal' q
                               errOrAvailable = decimal' a
                               errOrUsed      = (-) <$> errOrQuota <*> errOrAvailable
                               errOrUsage     = Usage <$> errOrQuota <*> errOrUsed <*> errOrAvailable
                           in over _Left UsageNotParsable errOrUsage





-- | extract how many days left
--
--   <tr class="t-row">
--     <td class="t-label">Laufzeitende</td>
--     <td>27.01.17</td>
--     <td>&nbsp;</td>
--   </tr>
--
extractDaysLeft :: Day -> [Tag LByteString] -> Either AppError Integer
extractDaysLeft today tags = firstTagTextBy "Days left" locator  tags >>= extract
    where locator = drop 4 . dropWhile (~/== "Laufzeitende")
          extract (toS -> s) = bimap EndDateNotParsable calcDaysLeft $ parseTimeM True defaultTimeLocale "%d.%m.%y" s
          calcDaysLeft end = diffDays end today





-- | Data.Text.Read.decimal wrapper
--
--   * skip's the rest content after parsing
--   * pack the error msg in 'Text'
--
decimal' :: Text -> Either Text Int
decimal' = bimap T.pack (view _1) . decimal


-- | Data.Text.Read.rational wrapper
--
--   * skip's the rest content after parsing
--   * pack the error msg in 'Text'
--
rational' :: Text -> Either Text Float
rational' = bimap T.pack (view _1) . rational



-- | Find the first Tag Text from the given Tag's with the given locator function
--
firstTagTextBy :: Text -> ([Tag LByteString] -> [Tag LByteString]) -> [Tag LByteString] -> Either AppError LByteString
firstTagTextBy id locator tags = firstTag (locator tags) >>= tagText
    where firstTag = maybe (Left $ TagNotFound id) Right . listToMaybe
          tagText = maybe (Left $ TagHasNoText id) Right . maybeTagText



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
                 where tryPost :: Postable a => WS.Session -> Text -> a -> IO (Either SomeException (Response LByteString))
                       tryPost sess url payload = try $ WS.post sess url payload
                       extractToken :: [Tag LByteString] -> Maybe LByteString
                       extractToken = fmap (fromAttrib "value") . listToMaybe . dropWhile (~/== "<input name=_csrf_token")




-- | (~/==) Wrapper to fix the selector Type to [Char].
--
-- TagSoup has two Type Classes for the selector: [Char] and (TagRep (Tag str)).
-- With the use of 'OverloadedText', we get ambiguous Type Classes for the selector.
--
(~/==) :: StringLike str => Tag str -> [Char] -> Bool
(~/==) a b = a ~/= (b :: [Char])
