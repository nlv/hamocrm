{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import Control.Monad.Trans.Except
-- import Control.Monad
import Data.Text
import Data.ByteString as B
-- import Data.String
import qualified Data.ByteString.Char8 as BS
-- import qualified Data.Text.Encoding.Base64 as B64
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Network.HTTP.Types.Method
import Servant
import System.IO

import Options.Applicative
import Data.Semigroup ((<>))

import Api

tokenFile = "token.txt"

data Options = Options {
    optUser :: String
  , optPort :: Int
  }

api :: Proxy Api
api = Proxy

optParser :: Parser Options
optParser = Options
        <$> strOption   ( long "user" <> short 'u' <> help "AmoCRM user")
        <*> option auto ( long "port" <> short 'p' <> help "Service port")

main :: IO ()
main = do
    h <- openFile tokenFile ReadMode 
    token <- BS.hGetLine h
    hClose h
    opts <- execParser opts'
    runIt opts token
    where opts' = info (optParser <**> helper)
                    ( fullDesc
                    <> progDesc "AmoCRM integration service"
                    <> header "AmoCRM integration service")

runIt :: Options -> ByteString -> IO ()
runIt opts token = do
  let port = optPort opts
      settings =
        setPort port $
        setBeforeMainLoop (BS.hPutStrLn stderr $ BS.pack ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp (optUser opts) token

mkApp :: String -> ByteString -> IO Application
mkApp user token = return $ cors (const $ Just policy) $ provideOptions api $ serve api (server user token)
  where
    policy = simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ], corsMethods = [methodGet, methodPost, methodDelete, methodOptions] }






