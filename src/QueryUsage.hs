{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module QueryUsage where

import           Control.Lens
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import           Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.List                  (isInfixOf)
import           Network.Wreq
import qualified Network.Wreq.Session       as WS
import           Network.Wreq.Types         (Postable (..))
import           Text.XML.Light
import           Types


queryUsage :: App Usage
queryUsage = withSession $ \sess -> do
               res <- lift $ WS.get sess "https://prepaidkundenbetreuung.eplus.de/content/prepaid/cfi56baf7pgl63d4/de/tarif/mein_tarif.htx"
               let body = res ^. responseBody
               either throwE return $ parse body


parse :: BL.ByteString -> Either AppError Usage
parse bs = parse bs >>= findUsage >>= extractUsage
    where parse :: BL.ByteString -> Either AppError Element
          parse bs = maybe (Left BodyNotParsable) Right $ parseXMLDoc bs
          findUsage :: Element -> Either AppError Element
          findUsage xml = maybe (Left UsageNotFound) Right $ filterElement (hasAttrVal "class" "usage") xml
          extractUsage :: Element -> Either AppError Usage
          -- <div ...>3975 von 5120 MB</div> -> Usage
          extractUsage (words . strContent -> [a, _, q, _]) = Right $ Usage (read q) (read q - read a) (read a)
          extractUsage _ = Left UsageNotExtractable


hasAttrVal :: String -> String -> Element -> Bool
hasAttrVal attr val e = maybe False (isInfixOf val) $ findAttr (unqual attr) e


withSession :: (WS.Session -> ExceptT AppError IO a) -> App a
withSession f = do
  providerLogin <- asks acProviderLogin
  lift $ (ExceptT $ login providerLogin) >>= f


login :: ProviderLogin -> IO (Either AppError WS.Session)
login pl = WS.withSession $ \sess -> do
             -- invalidate old session
             _ <- WS.post sess "https://prepaidkundenbetreuung.eplus.de/content/prepaid/cfi56baf7pgl63d4/de.invalidate.htx" BL.empty
             -- perform login
             res <- WS.post sess "https://prepaidkundenbetreuung.eplus.de/content/prepaid/cfi56baf7pgl63d4/de.login.htx" pl
             let body = BL.unpack $ res ^. responseBody
             return $ if isInfixOf "returnMessage" body
                      then Right sess
                      else Left $ LoginError body



instance Postable ProviderLogin where
    postPayload pl = postPayload [ "myaction"               := ("login" :: String)
                                 , "uid"                    := plUser pl
                                 , "pwd"                    := plPass pl
                                 , "forwardUrl"             := ("" :: String)
                                 , "email_verifcation_code" := ("" :: String)
                                 ]
