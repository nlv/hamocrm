{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Amocrm
    ( getList
    , getStatusesList
    , TokenRequest(..)
    ,OAuth2(..)
    , refreshToken
    ) where

import GHC.Generics
import           Data.Aeson
import qualified Data.List as L (length) 
import           Data.Aeson.Key
import           Data.Aeson.Types
import           Data.Aeson.Parser           (json)
import           Data.Conduit                (($$))
import           Data.Conduit.Attoparsec     (sinkParser)
import           Network.HTTP.Types.Header
import           Network.HTTP.Client         
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.HTTP.Types.Status   (statusCode)

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.CaseInsensitive
import qualified Data.ByteString.Char8 as BS

-- !! Убрать лишние импорты, объявления функций, констант, раширений. Проверить все предупреждения
-- !! Оптимизация


import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens hiding ((.=))

import Data.Text (Text, unpack)
import Data.ByteString
import Data.Vector (toList)

import Data.Amocrm

data OAuth2 = OAuth2 {
    access_token  :: String
  , refresh_token :: String
  , expires_in     :: Integer
} deriving (Generic, Show)

class TokenRequest a where
  tokUser      :: a -> String
  tokClientId     :: a -> String
  tokClientSecret :: a -> String
  tokRedirectURI  :: a -> String


instance FromJSON OAuth2
instance ToJSON OAuth2

-- !! Сделать stream

getList :: AmocrmModule a => String -> String -> [(ByteString, Maybe ByteString)] -> String -> ByteString -> IO (Either String (ListFromAmocrm a))
getList ename path queryParams user token = go 1 0

    where 
      go n total = do
        liftIO $ Prelude.putStrLn $ "Порция: " ++ show n
        r' <- getListPage ename path queryParams user token n
        case r' of
          err@(Left _) -> pure err
          Right r'' -> do 
            let here = L.length (r'' ^. els)
            liftIO $ Prelude.putStrLn $ "ВСЕГО: " ++ show (total + here)
            if here < 250
              then 
                pure (Right r'') 
              else do
                l2 <- go (n + 1) (total + here)
                case l2 of
                  err2@(Left _) -> pure err2
                  Right l3 -> pure (Right $ append' r'' l3)


getListPage :: AmocrmModule a => String -> String -> [(ByteString, Maybe ByteString)] -> String -> ByteString -> Int -> IO (Either String (ListFromAmocrm a))
getListPage ename path queryParams user token page = do

    let query = [
                  ("filter[pipeline_id]", Just $ BS.pack $ show pipeline_id)
                , ("order[created_at]", Just "desc")
                , ("limit", Just "250")
                , ("page", Just $ BS.pack $ show page)
                , ("filter[statuses][0][pipeline_id]", Just $ BS.pack $ show pipeline_id)
                ] ++ queryParams

    liftIO $ Prelude.putStrLn $ "===== QUERY PARAMS"
    liftIO $ Prelude.putStrLn $ show query
    liftIO $ Prelude.putStrLn $ "===== END QUERY PARAMS"
    manager <- newManager tlsManagerSettings
    request <- (setQueryString query . applyBearerAuth token) <$> parseRequest ("https://" ++ user ++ ".amocrm.ru/api/v4/" ++ path)

    liftIO $ Prelude.putStrLn $ "===== REQUEST"
    liftIO $ Prelude.putStrLn $ show request
    liftIO $ Prelude.putStrLn $ "===== END REQUEST"    

    withResponse request manager $ \response -> do
      v <- bodyReaderSource (responseBody response) $$ sinkParser json
      -- liftIO $ Prelude.putStrLn $ "====="
      -- liftIO $ Prelude.putStrLn $ show v
      -- liftIO $ Prelude.putStrLn $ "====="
      pure $ parseEither (parseList ename) v


parseList :: AmocrmModule a => String -> Value -> Parser (ListFromAmocrm a)
parseList  ename = 
  withObject "response" $ \obj -> do
    embedded <- (obj .: "_embedded")
    els <- embedded .: (fromString ename)
    ListFromAmocrm <$> toList <$> withArray ename (mapM parseElement) els

  where 
    parseElement :: AmocrmModule a => Value -> Parser a
    parseElement = 
      withObject "element" parser

getStatusesList :: Int -> String -> ByteString -> IO (Either String (ListFromAmocrm Status))
getStatusesList pipeline_id user token = do

    manager <- newManager tlsManagerSettings
    request <- (applyBearerAuth token) <$> parseRequest ("https://" ++ user ++ ".amocrm.ru/api/v4/leads/pipelines/" ++ (show pipeline_id) ++ "/statuses")

    withResponse request manager $ \response -> do
      v <- bodyReaderSource (responseBody response) $$ sinkParser json
      -- liftIO $ Prelude.putStrLn $ "====="
      -- liftIO $ Prelude.putStrLn $ show v
      -- liftIO $ Prelude.putStrLn $ "====="
      pure $ parseEither (parseList "statuses") v

refreshToken :: TokenRequest a => a -> OAuth2 -> IO (Either String OAuth2)
refreshToken tok oauth = do
    let requestObject = 
          object 
            [ "client_id" .= (tokClientId tok)
            , "client_secret" .= (tokClientSecret tok)
            , "grant_type" .= ("refresh_token" :: String)
            , "redirect_uri" .= (tokRedirectURI tok)
            , "refresh_token" .= (refresh_token oauth)
            ]

-- (HeaderName, ByteString)
-- ("CONTENT-TYPE" :: CI Text)

     
    -- liftIO $ LB.putStrLn $ encode requestObject
    manager <- liftIO $ newManager tlsManagerSettings
    request' <- parseRequest ("https://" ++ tokUser tok ++ ".amocrm.ru/oauth2/access_token")
    let request = request' { method = "POST", requestBody = RequestBodyBS $ LB.toStrict $ encode requestObject, requestHeaders = [(hContentType, "application/json")] }
    -- let request = addRequestHeader hContentType "application/json" request''
    -- let r = setRequestBody request "bbb"
    let RequestBodyBS a = requestBody request

    -- liftIO $ B.putStrLn a

    -- liftIO $ Prelude.putStrLn $ show request

    withResponse request manager $ \response -> do
      v <- bodyReaderSource (responseBody response) $$ sinkParser json
      liftIO $ Prelude.putStrLn $ "====="
      liftIO $ Prelude.putStrLn $ show v
      liftIO $ Prelude.putStrLn $ "====="      
      -- let r = parseEither parse v 
      -- LB.putStrLn $ encode v
      pure $ parseEither parseJSON v

-- getStatuses :: String -> ByteString -> Int -> IO (Either String (ListFromAmocrm Status))
-- getStatuses user token pipeline = do

--     -- let query = [
--     --               ("filter[pipeline_id]", Just $ BS.pack $ show pipeline_id)
--     --             , ("order[created_at]", Just "desc")
--     --             , ("limit", Just "250")
--     --             , ("page", Just $ BS.pack $ show page)
--     --             ]

--     manager <- newManager tlsManagerSettings
--     request <- applyBearerAuth token <$> parseRequest ("https://" ++ user ++ ".amocrm.ru/api/v4/leads/pipelines/" ++ show pipeline ++ "/statuses")

--     withResponse request manager $ \response -> do
--       v <- bodyReaderSource (responseBody response) $$ sinkParser json
--       -- liftIO $ Prelude.putStrLn $ "====="
--       -- liftIO $ Prelude.putStrLn $ show v
--       -- liftIO $ Prelude.putStrLn $ "====="
--       pure $ parseEither parseStatuses v


-- parseStatuses:: Value -> Parser (ListFromAmocrm Status)
-- parseStatuses = 
--   withObject "response" $ \obj -> do
--     embedded <- (obj .: "_embedded")
--     els <- embedded .: (fromString ename)
--     ListFromAmocrm <$> toList <$> withArray ename (mapM parseElement) els

--   where 
--     parseElement :: AmocrmModule a => Value -> Parser a
--     parseElement = 
--       withObject "element" parser      