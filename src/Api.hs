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
import Control.Monad.Reader

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
     , srToken :: ByteString
}

serverT :: ServerT Api (ReaderT ServerReader IO)
serverT = getAny "leads" :<|> getAny "users"

-- server :: String -> ByteString -> ServerT Api (ReaderT ServerReader IO)
server :: String -> ByteString -> Server Api
server user token = hoistServer api (readerToHandler user token) serverT

api :: Proxy Api
api = Proxy


-- getAny :: (AmocrmModule a) => String -> (ReaderT ServerReader (ExceptT ServerError IO)) [a]
getAny :: (AmocrmModule a) => String -> (ReaderT ServerReader IO) [a]
getAny mod = do
     user <- asks srUser
     token <- asks srToken
     leads' <- liftIO $ getList mod user token
     case leads' of
          Right leads -> pure $ leads ^. els
          Left err -> do
               liftIO $ Prelude.putStrLn $ show err
               error $ show err
               -- lift $ throwError err404
               -- pure [] --err404          


readerToHandler :: String -> ByteString -> ReaderT ServerReader IO a -> Handler a
readerToHandler user token r = do
     a <- liftIO (runReaderT r ServerReader {srUser = user, srToken = token})
     pure a


