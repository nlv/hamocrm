{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (
  Api
  ,server
  )  where

import Control.Monad.IO.Class

import Servant

import Data.ByteString
import Control.Lens
-- import Control.Monad.Reader
import Control.Monad.RWS hiding (getAny)
import Control.Monad.Except

import Data.Amocrm
import Network.Amocrm

type Api = LeadsApi :<|> UsersApi

type LeadsApi =
     "leads" :> (
          Get '[JSON] [Lead]
     ) 

type UsersApi =      
     "users" :> (
          Get '[JSON] [User]
     )      

data ServerReader = ServerReader {
       srUser  :: String
}

data ServerState = ServerState {
       ssToken :: ByteString
}

type ServerLog = String

type ServerMonad = RWST ServerReader ServerLog ServerState (ExceptT ServerError IO)

serverT :: ServerT Api ServerMonad
serverT = getAny "leads" :<|> getAny "users"

server :: String -> ByteString -> Server Api
server user token = hoistServer api (readerToHandler user token) serverT

api :: Proxy Api
api = Proxy


getAny :: (AmocrmModule a) => String -> ServerMonad [a]
getAny mod = do
     user <- asks srUser
     token <- ssToken <$> get
     leads' <- liftIO $ getList mod user token
     case leads' of
          Right leads -> pure $ leads ^. els
          Left err -> do
               liftIO $ Prelude.putStrLn $ show err
               throwError err404


readerToHandler :: String -> ByteString -> ServerMonad a -> Handler a
readerToHandler user token r = do
     let initR = ServerReader {srUser = user} 
         initS = ServerState { ssToken = token}
     a <- liftIO (runExceptT $  runRWST r initR initS)
     case a of
       Right (a, s, w) -> pure a
       Left err -> throwError err


