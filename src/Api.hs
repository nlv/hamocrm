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

type Api = LeadsApi -- :<|> RowApi :<|> ImageApi

type LeadsApi =
     "leads" :> (
          Get '[JSON] [Lead]
     )

server :: String -> ByteString -> Server Api
server user token = getLeads user token


getLeads :: String -> ByteString -> Handler [Lead]
getLeads user token = do
     leads' <- liftIO $ getList "leads" user token
     case leads' of
          Right leads -> pure $ leads ^. els
          Left _ -> throwError err404


