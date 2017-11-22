{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Exchange.Psql where

import Prelude

import Data.Profunctor.Product (p3)
import Data.Profunctor.Product.Default (def)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Opaleye ( Query
               , Column
               , Table(Table)
               , required
               , optional
               , (.==)
               , (.<)
               , runQuery
               , restrict)
import Control.Arrow (returnA)
import Control.Monad
import Data.Char
import Data.Functor ((<$>), fmap)
import Data.Int (Int64)
import Data.Map (Map, empty, size, mapKeys, toList, assocs)
import Data.Monoid
import Data.Time.LocalTime
import Data.UUID
import Database.PostgreSQL.Simple.Internal (Connection)
import Opaleye.Manipulation
import Opaleye.Internal.TableMaker (tableColumn)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Opaleye.Constant as C
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.PGTypes as P
import qualified Opaleye.Table as T

import Types.Exchange

import DB.Psql

exchangeTableStr :: String
exchangeTableStr = "create table if not exists exchanges"
       <> " ( name text not null primary key"
       <> ", timeZone text not null"
       <> ", timeZoneOffset int4 not null"
       <> " );"

type ExchangeColumn = Exchange' (Column P.PGText) (Column P.PGText) (Column P.PGInt4)

$(makeAdaptorAndInstance "pExchange" ''Exchange')

-- http://haskell.vacationlabs.com/en/latest/docs/opaleye/basic-db-mapping.html

exchangeTable :: Table ExchangeColumn ExchangeColumn
exchangeTable = T.Table "exchanges" (pExchange Exchange' { name = required "name"
                                          , timeZone = required "timezone"
                                          , timeZoneOffset = required "timezoneoffset"
                                          })

exchangeQuery :: Query ExchangeColumn
exchangeQuery = T.queryTable exchangeTable

exchangeExample :: IO [Exchange]
exchangeExample = do
  conn <- getPsqlConnection commonFilePath
  tups <- runQuery conn exchangeQuery
  closePsqlConnection conn
  return tups

exchangeToPsql :: Exchange -> ExchangeColumn
exchangeToPsql exchange = Exchange'
                          (P.pgString (name exchange))
                          (P.pgString (timeZone exchange))
                          (P.pgInt4 (timeZoneOffset exchange))


insertExchange :: Exchange -> Connection -> IO Int64
insertExchange exchange connection =
  runInsertMany connection exchangeTable [exchangeToPsql exchange]

-- insertExchange :: Exchange' a b c -> Connection -> IO Int64
-- insertExchange exchange connection =
--   runInsertMany connection exchangeTable [exchange]

-- nasdaq :: Exchange
-- nasdaq = Exchange' "NASDAQ" (TimeZone (-300) False "US/Eastern") (-300)

-- nyse :: Exchange
-- nyse = Exchange' "NYSE" (TimeZone (-300) False "US/Eastern") (-300)

nasdaq :: Exchange
nasdaq = Exchange' "NASDAQ" "US/Eastern" (-300)

nyse :: Exchange
nyse = Exchange' "NYSE" "US/Eastern" (-300)

  
exchanges = [nasdaq, nyse]

