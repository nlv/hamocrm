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
import Data.Time.Clock.POSIX
import Data.Aeson
import Data.ByteString as B
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

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


type Api = LeadsApi :<|> UsersApi :<|> PipelinesApi

type LeadsApi =
     "leads" :> (
          QueryParam "user" Int
       :> Get '[JSON] [Lead]
     ) 

type UsersApi =      
     "users" :> (
          Get '[JSON] [User]
     ) 

type PipelinesApi =
     "pipelines" :> (
          "statuses" :> Get '[JSON] [Status]
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
serverT = getLeads :<|> getUsers :<|> getStatuses pipeline_id

server :: ProgOptions -> Server Api
server opts = hoistServer api (readerToHandler opts) serverT

api :: Proxy Api
api = Proxy

getLeads :: Maybe Int -> ServerMonad [Lead]
getLeads (Just user) = getAny "leads" [("filter[responsible_user_id]", Just $ BS.pack $ show user)]
getLeads _           = getAny "leads" []

getUsers :: ServerMonad [User]
getUsers = getAny "users" []


-- !!! Дублирование получения токена с getAmy 
-- getAny :: (AmocrmModule a) => String -> ServerMonad [a]
getStatuses :: Int -> ServerMonad [Status]
getStatuses pipeline_id = do
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

-- getStatusesList :: Int -> String -> ByteString -> IO (Either String (ListFromAmocrm Status))
     statuses' <- liftIO $ getStatusesList pipeline_id (optUser opts) tsTokens'
     case statuses' of
          Right statuses -> pure $ statuses ^. els
          Left err -> do
               liftIO $ Prelude.putStrLn $ show err
               throwError err404

getAny :: (AmocrmModule a) => String -> [(ByteString, Maybe ByteString)] -> ServerMonad [a]
getAny mod queryParams = do
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
            
     leads' <- liftIO $ getList mod queryParams (optUser opts) tsTokens'
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


