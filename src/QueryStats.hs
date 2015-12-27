{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module QueryStats where

import           Control.Lens
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import           Control.Monad.Trans.Reader (asks)
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.List                  (isInfixOf)
import           Data.Maybe                 (fromJust)
import           Network.Wreq
import qualified Network.Wreq.Session       as WS
import           Network.Wreq.Types         (Postable (..))
import           Text.XML.Light
import           Types

queryStats :: App String
queryStats = withSession $ \sess -> do
               res <- lift $ WS.get sess "https://prepaidkundenbetreuung.eplus.de/content/prepaid/cfi56baf7pgl63d4/de/tarif/mein_tarif.htx"
               let body = BL.unpack $ res ^. responseBody
               return $ maybe "not found" show (extract body)


extract bs = parseXMLDoc bs >>= filterElement (hasAttrVal "class" "progressbar-text")


hasAttrVal :: String -> String -> Element -> Bool
hasAttrVal attr val e = maybe False (isInfixOf val) $ findAttr (unqual attr) e


withSession :: (WS.Session -> ExceptT AppError IO a) -> App a
withSession f = do
  providerLogin <- asks acProviderLogin
  lift $ ExceptT $ login providerLogin >>= either undefined (runExceptT . f)
    where login :: ProviderLogin -> IO (Either AppError WS.Session)
          login pl = WS.withSession $ \sess -> do
                       -- invalidate old session
                       _ <- WS.post sess "https://prepaidkundenbetreuung.eplus.de/content/prepaid/cfi56baf7pgl63d4/de.invalidate.htx" BL.empty
                       -- perform login
                       res <- WS.post sess "https://prepaidkundenbetreuung.eplus.de/content/prepaid/cfi56baf7pgl63d4/de.login.htx" pl
                       let body = BL.unpack $ res ^. responseBody
                       return $ if isInfixOf "responseMessage" body
                                then Left $ LoginError body
                                else Right sess


instance Postable ProviderLogin where
    postPayload pl = postPayload [ "myaction"               := ("login" :: String)
                                 , "uid"                    := plUser pl
                                 , "pwd"                    := plPass pl
                                 , "forwardUrl"             := ("" :: String)
                                 , "email_verifcation_code" := ("" :: String)
                                 ]
