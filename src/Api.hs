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

server :: String -> ByteString -> Server Api
server user token = getAny "leads" user token :<|> getAny "users" user token


getAny :: (AmocrmModule a) => String -> String -> ByteString -> Handler [a]
getAny mod user token = do
     leads' <- liftIO $ getList mod user token
     case leads' of
          Right leads -> pure $ leads ^. els
          Left _ -> throwError err404          


