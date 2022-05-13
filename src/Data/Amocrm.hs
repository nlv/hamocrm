{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Amocrm ( 
    
  ListFromAmocrm(..)
, els

, Lead
, lid
, lname

, AmocrmModule(..)

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

class AmocrmModule a where
  parser     :: Object -> Parser a


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

data ListFromAmocrm a = ListFromAmocrm {
  _els :: [a]
} deriving (Generic, Show)

makeLenses ''Lead
makeLenses ''ListFromAmocrm

instance AmocrmModule Lead where
  parser = \obj -> Lead <$> obj .: "id" <*> obj .: "name"



  












