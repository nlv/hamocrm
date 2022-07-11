{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DatatypeContexts #-}

module Api (
  Api
  ,server
  ,ProgOptions(..)
  )  where

import Control.Monad.IO.Class

import Servant

import Data.ByteString
import Control.Lens
-- import Control.Monad.Reader
import Control.Monad.RWS hiding (getAny)
import Control.Monad.Except

import System.IO
import GHC.Generics
import Data.Time
-- import Data.Time.Compat 
import Data.Time.Clock.POSIX
import Data.Aeson
import Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Data.Time.Format.ISO8601

import Data.Amocrm
import Network.Amocrm

-- !!! Не должно быть тут! 

tokenFile = "token.txt"

data ProgOptions = ProgOptions {
    optUser         :: String
  , optPort         :: Int
  , optClientId     :: String
  , optClientSecret :: String
  , optRedirectUri  :: String
  }

instance TokenRequest ProgOptions where
  tokUser         = optUser
  tokClientId     = optClientId
  tokClientSecret = optClientSecret
  tokRedirectURI  = optRedirectUri


type Api = LeadsApi :<|> UsersApi :<|> PipelinesApi :<|> EnumsApi

type LeadsApi =
     "leads" :> (
          QueryParam "user" Int
       :> QueryParam "status" Int   
       :> QueryParam "master" Int
       :> QueryParam "created_at_from" Day
       :> QueryParam "created_at_to" Day
       :> QueryParam "closed_date_from" Day
       :> QueryParam "closed_date_to" Day       
       :> Get '[JSON] [Lead]
     ) 

type UsersApi =      
     "users" :> (
          Get '[JSON] [User]
     ) 

type PipelinesApi =
     "pipelines" :> (
          Get '[JSON] [Pipeline]
     ) 

type EnumsApi =
     "enums" :> (
          Capture "enum id" Int
       :> Get '[JSON] [FieldEnum]
     )           

data TokensStamp = TokensStamp {
  tsTokens       :: OAuth2
, tsEndTokenTime :: POSIXTime
} deriving (Show, Generic)

instance FromJSON TokensStamp
instance ToJSON TokensStamp

data ServerReader = ServerReader {
       srOptions  :: ProgOptions
     , srTokenFile :: FilePath  
}

data ServerState = ServerState {
       ssTokensStamps :: Maybe TokensStamp
}

type ServerLog = String

type ServerMonad = RWST ServerReader ServerLog ServerState (ExceptT ServerError IO)

serverT :: ServerT Api ServerMonad
serverT = getLeads :<|> getUsers :<|> getPipelines :<|> getEnums

server :: ProgOptions -> Server Api
server opts = hoistServer api (readerToHandler opts) serverT

api :: Proxy Api
api = Proxy

getLeads :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Day -> Maybe Day -> Maybe Day -> Maybe Day -> ServerMonad [Lead]
getLeads user status master created_at_from created_at_to closed_date_from closed_date_to = do
     res <- getAny 
          "leads" 
          "leads" $ 
          p2p "filter[responsible_user_id]" (fmap show user) (BS.pack) 
            ++ p2p "filter[statuses][0][status_id]" (fmap show status) (BS.pack) 
            ++ p2p "filter[created_at][from]" (fmap showGregorian created_at_from) (BS.pack)
            ++ p2p "filter[created_at][to]" (fmap showGregorian created_at_to) (BS.pack)
            -- !! Зашитый код списка мастеров
          --   ++ p2p "filter[custom_fields_values][1143523][]" master (BS.pack . show) 
          --   ++ p2p "filter[custom_fields_values][1143523]" master (BS.pack . show) 
          --   ++ p2p "filter[1143523]" master (BS.pack . show) 
          --   ++ p2p "filter[1143523][]" master (BS.pack . show) 
          --   ++ p2p "filter[custom_fields_values][Мастер][]" master (BS.pack . show) 
          --   ++ p2p "filter[custom_fields_values][Мастер]" master (BS.pack . show) 
          --   ++ p2p "filter[1143523][values]" master (BS.pack . show) 
          --   ++ p2p "filter[1143523][values][]" master (BS.pack . show) 

     let res' = case master of
          Just master' -> Prelude.filter (\l -> l ^. lmaster == master) res
          _            -> res

     let res'' = case closed_date_from of
          Just closed_date_from' -> Prelude.filter (\l -> maybe False id $ fmap (\d -> utctDay d >= closed_date_from') (l ^. lclosedDate)) res'          
          _            -> res'

     let res''' = case closed_date_to of
          Just closed_date_to' -> Prelude.filter (\l -> maybe False id $ fmap (\d -> utctDay d < closed_date_to') (l ^. lclosedDate)) res''
          _            -> res''

     pure res'''
            
     where 
          p2p :: ByteString -> Maybe String -> (String -> ByteString) -> [(ByteString, Maybe ByteString)]
          p2p key (Just param) conv = [(key, Just $ conv param)]
          p2p key _ _            = []
          -- statuses' s = BS.pack (show  s)
          statuses' s = "[status_id][" `BS.append` BS.pack (show  s) `BS.append` "]"

getUsers :: ServerMonad [User]
getUsers = getAny "users" "users" []

getPipelines :: ServerMonad [Pipeline]
getPipelines = getAny "pipelines" "leads/pipelines" []

getEnums :: Int -> ServerMonad [FieldEnum]
getEnums enumid = do
     opts <- asks srOptions
     tokensStamp' <- ssTokensStamps <$> get
     tokenFile <- asks srTokenFile
     TokensStamp{..} <- 
          case tokensStamp' of
               Just o -> pure o
               Nothing -> do
                    h <- liftIO $ openFile tokenFile ReadMode 
                    tokens' <- liftIO $ eitherDecodeStrict <$> B.hGetLine h :: ServerMonad (Either String TokensStamp)
                    liftIO $ hClose h
                    case tokens' of
                         Left err -> do
                              liftIO $ Prelude.putStrLn $ show err
                              throwError err404
                         Right tokens -> pure tokens

     now <- liftIO getPOSIXTime
     liftIO $ Prelude.putStrLn $ show now
     liftIO $ Prelude.putStrLn $ show tsEndTokenTime
     let expired = now > tsEndTokenTime

     tsTokens' <- if expired 
          then do
            liftIO $ System.IO.putStrLn $ "Надо менять"  
            newToken' <- liftIO $ refreshToken opts tsTokens
            liftIO $ Prelude.putStrLn $ show newToken'
            case newToken' of
              Left err -> do
                   liftIO $ Prelude.putStrLn $ show err
                   throwError err404
              Right newToken -> do
                let newTokensStamp = TokensStamp { tsTokens = newToken, tsEndTokenTime = (fromInteger . expires_in) tsTokens + now}
                h <- liftIO $ openFile tokenFile WriteMode 
                let newTS = BL.toStrict $ encode newTokensStamp
                liftIO $ BS.putStrLn newTS    
                liftIO $ BS.hPutStrLn h newTS
                liftIO $ hClose h
                put $ ServerState {ssTokensStamps = Just newTokensStamp}
                pure $ BS.pack $ access_token newToken
          else do
            liftIO $ System.IO.putStrLn $ "НЕ надо менять"  
            pure $ BS.pack $ access_token tsTokens          

     enums' <-  liftIO $ getEnumsList enumid (optUser opts) tsTokens'
     case enums' of
          Right enums -> pure enums
          Left err -> do
               liftIO $ Prelude.putStrLn $ show err
               throwError err404

-- !! Проверку токенов вынести отдельно, а не дублировать. Может ее в middleware можно вынести?


-- !! делать специфические компинаторы для парсинга одинаковых по структуре частей ответа (parseEmbedded, например),
-- !! а не через getList все пытаться распарсить


getAny :: (AmocrmModule a) => String -> String -> [(ByteString, Maybe ByteString)] -> ServerMonad [a]
getAny mod path queryParams = do
     opts <- asks srOptions
     tokensStamp' <- ssTokensStamps <$> get
     tokenFile <- asks srTokenFile
     TokensStamp{..} <- 
          case tokensStamp' of
               Just o -> pure o
               Nothing -> do
                    h <- liftIO $ openFile tokenFile ReadMode 
                    tokens' <- liftIO $ eitherDecodeStrict <$> B.hGetLine h :: ServerMonad (Either String TokensStamp)
                    liftIO $ hClose h
                    case tokens' of
                         Left err -> do
                              liftIO $ Prelude.putStrLn $ show err
                              throwError err404
                         Right tokens -> pure tokens

     now <- liftIO getPOSIXTime
     liftIO $ Prelude.putStrLn $ show now
     liftIO $ Prelude.putStrLn $ show tsEndTokenTime
     let expired = now > tsEndTokenTime

     tsTokens' <- if expired 
          then do
            liftIO $ System.IO.putStrLn $ "Надо менять"  
            newToken' <- liftIO $ refreshToken opts tsTokens
            liftIO $ Prelude.putStrLn $ show newToken'
            case newToken' of
              Left err -> do
                   liftIO $ Prelude.putStrLn $ show err
                   throwError err404
              Right newToken -> do
                let newTokensStamp = TokensStamp { tsTokens = newToken, tsEndTokenTime = (fromInteger . expires_in) tsTokens + now}
                h <- liftIO $ openFile tokenFile WriteMode 
                let newTS = BL.toStrict $ encode newTokensStamp
                liftIO $ BS.putStrLn newTS    
                liftIO $ BS.hPutStrLn h newTS
                liftIO $ hClose h
                put $ ServerState {ssTokensStamps = Just newTokensStamp}
                pure $ BS.pack $ access_token newToken
          else do
            liftIO $ System.IO.putStrLn $ "НЕ надо менять"  
            pure $ BS.pack $ access_token tsTokens     
            
     leads' <- liftIO $ getList mod path queryParams (optUser opts) tsTokens'
     case leads' of
          Right leads -> pure $ leads ^. els
          Left err -> do
               liftIO $ Prelude.putStrLn $ show err
               throwError err404


readerToHandler :: ProgOptions -> ServerMonad a -> Handler a
readerToHandler opts r = do
     let initR = ServerReader {srOptions = opts, srTokenFile = tokenFile} 
         initS = ServerState { ssTokensStamps = Nothing}
     a <- liftIO (runExceptT $  runRWST r initR initS)
     case a of
       Right (a, s, w) -> pure a
       Left err -> do 
            liftIO $ Prelude.putStrLn $ show err
            throwError err


