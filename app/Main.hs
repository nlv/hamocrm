{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}


module Main where

import GHC.Generics
import Control.Monad.IO.Class
import Data.Text
import Data.Time
import Data.Time.Clock.POSIX
import Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Network.HTTP.Types.Method
import Servant
import System.IO
import Control.Monad.Trans.Except



import Options.Applicative
import Data.Semigroup ((<>))

import Data.Aeson

import Api
import Network.Amocrm

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

data TokensStamp = TokensStamp {
  tokens       :: OAuth2
, endTokenTime :: POSIXTime
} deriving (Show, Generic)

instance FromJSON TokensStamp
instance ToJSON TokensStamp


api :: Proxy Api
api = Proxy

optParser :: Parser ProgOptions
optParser = ProgOptions
        <$> strOption   ( long "user"     <> short 'u' <> help "AmoCRM user")
        <*> option auto ( long "port"     <> short 'p' <> help "Service port")
        <*> strOption   ( long "id"       <> short 'i' <> help "Client id")
        <*> strOption   ( long "secret"   <> short 's' <> help "Client secret")
        <*> strOption   ( long "redirect" <> short 'r' <> help "Redirect URI")

main :: IO ()
main = do
    opts <- execParser opts'
    tokens' <- getAccessToken opts
    case tokens' of
      Left err -> System.IO.putStrLn err
      Right access_token -> runIt opts access_token
    where opts' = info (optParser <**> helper)
                    ( fullDesc
                    <> progDesc "AmoCRM integration service"
                    <> header "AmoCRM integration service")


getAccessToken :: ProgOptions -> IO (Either String ByteString)
getAccessToken opts = do
    h <- liftIO $ openFile tokenFile ReadMode 
    tokens' <- eitherDecodeStrict <$> B.hGetLine h :: IO (Either String TokensStamp)
    liftIO $ hClose h
    case tokens' of
      Left err -> pure $ Left err
      Right tt@TokensStamp{..} -> do
        now <- getPOSIXTime
        liftIO $ Prelude.putStrLn $ show now
        liftIO $ Prelude.putStrLn $ show endTokenTime
        let expired = now > endTokenTime
        if expired 
          then do
              liftIO $ System.IO.putStrLn $ "Надо менять"  
              pure $ Right $ BS.pack $ access_token tokens
              newToken' <- refreshToken opts tokens
              liftIO $ Prelude.putStrLn $ show newToken'
              case newToken' of
                Left err -> pure $ Left err
                Right newToken -> do
                  let newTokensStamp = TokensStamp { tokens = newToken, endTokenTime = (fromInteger . expires_in) tokens + now}
                  h <- liftIO $ openFile tokenFile WriteMode 
                  liftIO $ BS.hPutStrLn h $ BL.toStrict $ encode newTokensStamp
                  liftIO $ hClose h
                  pure $ Right $ BS.pack $ access_token newToken
          else do
            liftIO $ System.IO.putStrLn $ "НЕ надо менять"  
            pure $ Right $ BS.pack $ access_token tokens

   
runIt :: ProgOptions -> ByteString -> IO ()
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


