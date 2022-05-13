{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
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

import Control.Applicative
import Control.Monad
import Control.Lens

import Data.Text (unpack)

import Data.Amocrm
import Network.Amocrm

userFile = "user.txt"
tokenFile = "token.txt"

someFunc :: IO ()
someFunc = do
    token <- B.readFile tokenFile
    user <- readFile userFile

    a <- getList "leads" user token

    case a of
        Right leads -> mapM_ putStrLn $ map (\e -> show (e ^. lid) ++ " ;;; " ++ unpack (e ^. lname)) $ leads ^. els
        Left err -> putStrLn $ show err

