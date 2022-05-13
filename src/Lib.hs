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

import Data.AmoCRM

userFile = "user.txt"
tokenFile = "token.txt"

someFunc :: IO ()
someFunc = do
    token <- B.readFile tokenFile
    user <- readFile userFile

    manager <- newManager tlsManagerSettings
    request <- applyBearerAuth token <$> parseRequest ("https://" ++ user ++ ".amocrm.ru/api/v4/leads")

    withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

        v <- bodyReaderSource (responseBody response) $$ sinkParser json

        let a = (parseEither (parseListFromAmoCRM "leads" parseLeadFromAmoCRM) v) :: Either String (ListFromAmoCRM Lead)
        case a of
            Right leads -> mapM_ putStrLn $ map (\e -> show (e ^. lid) ++ " ;;; " ++ unpack (e ^. lname)) $ _els leads
            Left err -> putStrLn $ show err

