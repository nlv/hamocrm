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

user = "artandem7"

token :: B.ByteString
token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJSUzI1NiIsImp0aSI6ImI3YTU2MmUxNWRiMmJjYjZhYjNlNWQ3ZDA5ZjgxNTQ2ZGNmZGQ0ZDMzMjI1MjNhMTYyMWM1NmI3YjAyN2M2MTNkMWJmMzgwNWNhYjcxZDBmIn0.eyJhdWQiOiI1N2ZlZjgyNy1hNDNlLTQ2NGQtOGU0My1mYzQyOGJjMzBlMmQiLCJqdGkiOiJiN2E1NjJlMTVkYjJiY2I2YWIzZTVkN2QwOWY4MTU0NmRjZmRkNGQzMzIyNTIzYTE2MjFjNTZiN2IwMjdjNjEzZDFiZjM4MDVjYWI3MWQwZiIsImlhdCI6MTY1MjQxODc3MSwibmJmIjoxNjUyNDE4NzcxLCJleHAiOjE2NTI1MDUxNzEsInN1YiI6Ijc4Nzk1OTEiLCJhY2NvdW50X2lkIjoyOTM3ODExOSwic2NvcGVzIjpbImNybSJdfQ.QzwZqA9QP94yfcGSKNgK1ots8vgGK0sT8owqPRXdDks1mOb-NcIq8je1Jo_US6S8FifVlPlym26hUF7gMp0DkRgWLxkA8D2eP2314Rcwq5eqgTifzEyDL1jym6EkXjklpbMf3NYqptFVw5OhvlHCBofzkEZIpA8S-ouokM38Z0lxZx3WfpSASyuMXjzzj4cZM7R7ZGkFcwtb_AbqynTve6JEO6AZbzRF0DmU91bJmNLhMvI26yAgVwmxwEb5VG3r96mlIOABBhRbBnQI0dgY7C1Sm36tb6k-0D70EcLVNOgvhnNlAXWyJtUiunIvr91PXUqBL4uWDdHLc5bFD3i_dg"

request' = applyBearerAuth token $ parseRequest_ $ "https://" ++ user ++ ".amocrm.ru/api/v4/leads" 

someFunc :: IO ()
someFunc = do
    manager <- newManager tlsManagerSettings
    request <- applyBearerAuth token <$> parseRequest ("https://" ++ user ++ ".amocrm.ru/api/v4/leads")

    -- request2 <- applyBearerAuth token <$> parseRequest ("https://" ++ user ++ ".amocrm.ru/api/v4/users")

    withResponse request manager $ \response -> do
        putStrLn $ "The status code was: " ++
                   show (statusCode $ responseStatus response)

        v <- bodyReaderSource (responseBody response) $$ sinkParser json

        let a = (parseEither (parseListFromAmoCRM "leads" parseLeadFromAmoCRM) v) :: Either String (ListFromAmoCRM Lead)
        case a of
            Right leads -> mapM_ putStrLn $ map (\e -> show (e ^. lid) ++ " ;;; " ++ unpack (e ^. lname)) $ _els leads
            Left err -> putStrLn $ show err

