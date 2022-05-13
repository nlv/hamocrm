{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.AmoCRM ( 
    
  ListFromAmoCRM
, parseList
, els

, Lead(..)
, parseLead
, lid
, lname

) where

import GHC.Generics
import Data.Time
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Key
import Data.Vector (toList)
import Control.Lens

import Control.Monad


data Lead = Lead {
  _lid           :: Integer
, _lname         :: Text
-- , _lresponsible  :: Text
-- , _laddress      :: Text
-- , _ldateVisit    :: UTCTime
-- , _ltype         :: Text
-- , _lsellCost     :: Float -- Цена клиенту
-- , _lpartsCost    :: Float -- Стоимость материалов
-- , _lworksCost    :: Float -- Стоимость работ
-- , _lofficeIncome :: Float -- Перевод в офис
-- , _lclosedDate   :: UTCTime -- Дата закрытия
-- , _lstatusId     :: Text
} deriving (Generic, Show)

data ListFromAmoCRM a = ListFromAmoCRM {
  _els :: [a]
} deriving (Generic, Show)

makeLenses ''Lead
makeLenses ''ListFromAmoCRM

parseList :: String -> (Value -> Parser a) -> Value -> Parser (ListFromAmoCRM a)
parseList ename parseElement = 
  withObject "List from amoCRM" $ \obj -> do
    embedded <- (obj .: "_embedded")
    els <- embedded .: (fromString ename)
    ListFromAmoCRM <$> toList <$> withArray "list of elements" (mapM parseElement) els

parseLead :: Value -> Parser Lead
parseLead = 
  withObject "lead" $ \obj -> do
    Lead <$> obj .: "id" <*> obj .: "name"

  












