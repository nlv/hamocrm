{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (
  Api
  )  where

import Data.Text
import Servant

import Data.Amocrm

type Api = LeadsApi -- :<|> RowApi :<|> ImageApi

type LeadsApi =
     "leads" :> (
          Get '[JSON] [Lead]
     )

-- server :: Server Api
-- server = getLeads


-- getLeads :: Handler [Lead]
-- getLeads = do
--   p' <- liftIO $ do
--     conn <- liftIO $ Pg.connectPostgreSQL $ dbConnectString opts
--     runBeamPostgresDebug putStrLn conn (B.getPostById id)
--   -- pure p'
--   case p' of
--     Just p'' -> do
--       p''' <- liftIO $ B.postToA (s3ConnInfo opts) p''
--       case p''' of
--         Left _ -> throwError err404
--         Right p -> pure p
--     _      -> throwError err404


