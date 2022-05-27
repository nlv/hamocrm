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

, User
, uid
, uname

, AmocrmModule(..)

) where

import GHC.Generics
import Data.Scientific
import Data.Time
import Data.Time.Clock.POSIX
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Key
import qualified Data.Aeson.Combinators.Decode as ACD
import qualified Data.Vector as V (map, Vector(..), toList, empty, length)
import Control.Lens

import Control.Monad

data ListFromAmocrm a = ListFromAmocrm {
  _els :: [a]
} deriving (Generic, Show)

makeLenses ''ListFromAmocrm

class AmocrmModule a where
  parser     :: Object -> Parser a

data Lead = Lead {
  _lid           :: Integer -- Номер 
, _lname         :: Text -- Название 
, _href          :: Text -- Ссылка на заявку
, _lresponsible  :: Integer -- Ответственный
, _laddress      :: Text -- Адрес
, _ldateVisit    :: Maybe UTCTime -- Дата выезда
, _ltype         :: Text
, _lsellCost     :: Float -- Цена клиенту
, _lpartsCost    :: Float -- Стоимость материалов, Затраты на материал, Затрачено
, _lworksCost    :: Float -- Стоимость работ для клиента
, _lofficeIncome :: Float -- Перевод в офис, Заработал Офис
, _lclosedDate   :: Maybe UTCTime -- Дата закрытия
, _lstatusId     :: Integer
} deriving (Generic, Show)

instance ToJSON Lead

makeLenses ''Lead

instance AmocrmModule Lead where
  parser = 
    \obj -> do
      -- let customObject :: ACD.Decoder (Text, Value)
      --     customObject = (,) <$> ACD.key "field_name" ACD.text <*> ACD.key "values" ACD.auto
      --     customArray :: ACD.Decoder [(Text, Value)]
      --     customArray = ACD.list customObject
      --     custom = ACD.parseEither customArray (Object obj)
      links <- obj .: "_links"
      self <- links .: "self"
      customFields <- obj .:? "custom_fields_values" .!= Array V.empty
      customFields' <- customDecoder customFields
      laddress <- getCustomText "address" "Адрес объекта" customFields'
      ldateVisit <- getCustomUTCTime "dateVisit" "Дата выезда" customFields'
      ltype <- getCustomText "type" "Тип заказа" customFields'
      lsellCost <- getCustomFloat "sellCost" "Цена для клиента" customFields'
      lpartsCost <- getCustomFloat "partsCost" "Затрачено" customFields'
      lworksCost <- getCustomFloat "worksCost" "Цена для клиента" customFields'
      lofficeIncome <- getCustomFloat "officeIncome" "Заработал Офис" customFields'
      lclosedDate <- getCustomUTCTime "closedDate" "Дата закрытия" customFields'

      Lead 
        <$> obj .: "id"
        <*> obj .: "name"
        <*> self .: "href"
        <*> obj .: "responsible_user_id"
        <*> pure laddress
        <*> pure ldateVisit
        <*> pure ltype
        <*> pure lsellCost
        <*> pure lpartsCost
        <*> pure lworksCost
        <*> pure lofficeIncome
        <*> pure lclosedDate
        <*> obj .: "status_id"

    where 
      getCustomText :: String -> Text -> [(Text, Value)] -> Parser Text
      getCustomText lname cname fields = withText lname pure $ maybe "" id $ lookup cname fields

      -- TODO read ...
      getCustomFloat :: String -> Text -> [(Text, Value)] -> Parser Float
      getCustomFloat lname cname fields = do
        text <- getCustomText lname cname fields
        pure $ if Data.Text.length text == 0 then 0 else (read $ unpack text)

      getCustomInt :: String -> Text -> [(Text, Value)] -> Parser (Maybe Int)
      getCustomInt lname cname fields = do
        r <- withScientific lname (pure . toBoundedInteger) $ maybe (Number 0) id $ lookup cname fields
        pure r

      getCustomUTCTime :: String -> Text -> [(Text, Value)] -> Parser (Maybe UTCTime)
      getCustomUTCTime lname cname fields = do
        posix <- getCustomInt lname cname fields
        case posix of
          Just 0 -> pure Nothing
          Just posix' -> pure $ Just $ posixSecondsToUTCTime (fromIntegral posix') 
          _ -> pure Nothing

      customDecoder (Array arr) = do
          let l =  mapM (parse p) (V.toList arr) 
          case l of
            Error err -> mzero
            Success l' -> pure l'
      customDecoder _ = mzero
      p = withObject "custom field" $ \obj -> do
        values <- obj .: "values"
        v <- p' values
        (,) <$> obj .: "field_name" <*> pure v

      p' = withArray "custom value" $ \arr -> do
        let values = V.toList arr
        let val = if Prelude.length values == 0 then "" else Prelude.head values
        r <- withObject "v''" (\obj -> obj .: "value") val
        pure r 
        

data User = User {
  _uid           :: Integer -- Номер заявки 
, _uname         :: Text
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

makeLenses ''User

instance ToJSON User

instance AmocrmModule User where
  parser = \obj -> User <$> obj .: "id" <*> obj .: "name"  



  












