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

, Status
, sid
, sname

, Pipeline
, pid
, pname
, pstatuses

, AmocrmModule(..)

, append'

, pipeline_id

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

import Data.Vector (toList)

data ListFromAmocrm a = ListFromAmocrm {
  _els :: [a]
} deriving (Generic, Show)


makeLenses ''ListFromAmocrm

append' l1 l2 = ListFromAmocrm (l1 ^. els ++ l2 ^. els)

class AmocrmModule a where
  parser     :: Object -> Parser a

data Lead = Lead {
  _lid           :: Integer -- Номер 
, _lname         :: Text -- Название 
, _href          :: Text -- Ссылка на заявку
, _lresponsible  :: Integer -- Ответственный (менеджер)
, _lmaster       :: Maybe Int -- Мастер
, _lmasterSalary :: Float -- ЗП мастера
, _laddress      :: Text -- Адрес
, _lcity         :: Text -- Город
, _ldateVisit    :: Maybe UTCTime -- Дата выезда
, _ltype         :: Text
, _lsellCost     :: Float -- Цена клиенту
, _lpartsCost    :: Float -- Стоимость материалов, Затраты на материал, Затрачено
, _lworksCost    :: Float -- Стоимость работ для клиента
, _lnetWorksCost :: Float -- Сумма работ
, _lofficeIncome :: Float -- Перевод в офис, Заработал Офис
, _lclosedDate   :: Maybe UTCTime -- Дата закрытия
, _lstatusId     :: Integer
} deriving (Generic, Show)

instance ToJSON Lead

makeLenses ''Lead




instance AmocrmModule Lead where
  parser = 
    \obj -> do
      links <- obj .: "_links"
      self <- links .: "self"
      customFields <- obj .:? "custom_fields_values" .!= Array V.empty
      customFields' <- customDecoder customFields
      lmaster <- getCustomEnum "master" "Мастер" customFields'
      lmasterSalary <- getCustomFloat "masterSalary" "Итого вознаграждение мастера" customFields'
      laddress <- getCustomText "address" "Адрес объекта" customFields'
      lcity <- getCustomText "city" "Город" customFields'
      ldateVisit <- getCustomUTCTime "dateVisit" "Дата выезда" customFields'
      ltype <- getCustomText "type" "Тип заказа" customFields'
      lsellCost <- getCustomFloat "sellCost" "Цена для клиента" customFields'
      lpartsCost <- getCustomFloat "partsCost" "Затрачено" customFields'
      lworksCost <- getCustomFloat "worksCost" "Цена для клиента" customFields'
      lnetWorksCost <- getCustomFloat "netWorksCost" "Сумма работ" customFields'
      lofficeIncome <- getCustomFloat "officeIncome" "Заработал Офис" customFields'
      lclosedDate <- getCustomUTCTime "closedDate" "Дата закрытия" customFields'

      Lead 
        <$> obj .: "id"
        <*> obj .: "name"
        <*> self .: "href"
        <*> obj .: "responsible_user_id"
        <*> pure lmaster
        <*> pure lmasterSalary
        <*> pure laddress
        <*> pure lcity
        <*> pure ldateVisit
        <*> pure ltype
        <*> pure lsellCost
        <*> pure lpartsCost
        <*> pure lworksCost
        <*> pure lnetWorksCost
        <*> pure lofficeIncome
        <*> pure lclosedDate
        <*> obj .: "status_id"

    where 
      getCustomText :: String -> Text -> [(Text, Value)] -> Parser Text
      getCustomText lname cname fields = maybe (pure "") (withObject lname (\obj -> obj .: "value" >>= withText lname (pure . id))) $ lookup cname fields

      -- TODO read ...
      getCustomFloat :: String -> Text -> [(Text, Value)] -> Parser Float
      getCustomFloat lname cname fields = do
        text <- getCustomText lname cname fields
        pure $ if Data.Text.length text == 0 then 0 else (read $ unpack text)

      getCustomInt :: String -> Text -> [(Text, Value)] -> Parser (Maybe Int)
      getCustomInt lname cname fields = maybe (pure Nothing) (withObject lname (\obj -> obj .: "value" >>= withScientific lname (pure . toBoundedInteger))) (lookup cname fields)

      getCustomEnum :: String -> Text -> [(Text, Value)] -> Parser (Maybe Int)
      getCustomEnum lname cname fields = do
        r <- maybe (pure Nothing) parseValue (lookup cname fields)
        pure r     

        where parseValue = withObject ("enum of " ++ lname) $ \obj -> do
                v <- obj .: "enum_id"
                withScientific ("enum_id of enum of " ++ lname) (pure . toBoundedInteger) v

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

-- !!! Возвращаем первый элемент, а надо все
      p' = withArray "custom value" $ \arr -> do
        let values = V.toList arr
        let val = if Prelude.length values == 0 then "" else Prelude.head values
        r <- withObject "v''" (pure . id) val
        pure val
        

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

pipeline_id :: Int
pipeline_id = 5023048

data Status = Status {
    _sname :: Text
  , _sid   :: Integer
}  deriving (Generic, Show)

makeLenses ''Status

instance ToJSON Status

instance AmocrmModule Status where
  parser = \obj -> Status <$> obj .: "name" <*> obj .: "id"  

data Pipeline = Pipeline {
    _pname     :: Text
  , _pid       :: Integer
  , _pstatuses :: [Status]
}  deriving (Generic, Show)

makeLenses ''Pipeline

instance ToJSON Pipeline

-- !!!  Здесь повтор parseList. одинакого вложенные элементы парсим
-- !!! с этим надо разобраться и проще парсинг сделать
instance AmocrmModule Pipeline where
  parser = \obj -> do
    embedded <- obj .: "_embedded"
    els <- embedded .: "statuses"
    statuses <- toList <$> withArray "statuses in pipeline" (mapM (withObject "status in pipeline" parser)) els
    Pipeline <$> obj .: "name" <*> obj .: "id" <*> pure statuses











