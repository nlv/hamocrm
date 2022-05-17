{-# LANGUAGE OverloadedStrings #-}

module Network.Amocrm
    ( getList
    ) where


import           Data.Aeson
import           Data.Aeson.Key
import           Data.Aeson.Types
import           Data.Aeson.Parser           (json)
import           Data.Conduit                (($$))
import           Data.Conduit.Attoparsec     (sinkParser)
import           Network.HTTP.Client         
import           Network.HTTP.Client.Conduit (bodyReaderSource)
import           Network.HTTP.Client.TLS     (tlsManagerSettings)
import           Network.HTTP.Types.Status   (statusCode)

import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens

import Data.Text (Text, unpack)
import Data.ByteString
import Data.Vector (toList)

import Data.Amocrm

getList :: AmocrmModule a => String -> String -> ByteString -> IO (Either String (ListFromAmocrm a))
getList ename user token = do

    manager <- newManager tlsManagerSettings
    request <- applyBearerAuth token <$> parseRequest ("https://" ++ user ++ ".amocrm.ru/api/v4/" ++ ename)

    withResponse request manager $ \response -> do
      v <- bodyReaderSource (responseBody response) $$ sinkParser json
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
