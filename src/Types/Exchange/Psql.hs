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

data Foo' a b c = Foo' { name :: a, timeZone :: b, timeZoneOffset :: c } deriving (Show)

type FooColumn = Foo' (Column P.PGText) (Column P.PGText) (Column P.PGInt4)

type Bar = Foo' String String Int

$(makeAdaptorAndInstance "pFoo" ''Foo')

-- http://haskell.vacationlabs.com/en/latest/docs/opaleye/basic-db-mapping.html

fooTable :: Table FooColumn FooColumn
fooTable = T.Table "exchanges" (pFoo Foo' { name = required "name"
                                          , timeZone = required "timezone"
                                          , timeZoneOffset = required "timezoneoffset"
                                          })

fooQuery :: Query FooColumn
fooQuery = T.queryTable fooTable

fooExample :: IO [Bar]
fooExample = do
  conn <- getPsqlConnection commonFilePath
  tups <- runQuery conn fooQuery
  closePsqlConnection conn
  return tups


--type ExchangeColumn = Exchange' (Column P.PGText) (Column P.PGText) (Column P.PGInt4)

--exchangeQuery :: Query  (Column P.PGText, Column P.PGText, Column P.PGInt4)

-- exchangeQuery :: Query ExchangeColumn
-- exchangeQuery = (\(namec, tzc, tzoc) -> Exchange' namec tzc tzoc) <$> T.queryTable exchangeTable

---- $(makeAdaptorAndInstance "pExchange" ''Exchange')

-- exchangesExample :: IO [Foo]
-- exchangesExample = do
--   conn <- getPsqlConnection commonFilePath
--   tups <- runQuery conn exchangeQuery
--   closePsqlConnection conn
--   return tups

-- insertExchange :: Exchange' a b c -> Connection -> IO Int64
-- insertExchange exchange connection =
--   runInsertMany connection exchangeTable [exchange]

--exchangesExample :: IO [(String, String, Int)]
-- exchangesExample :: IO [Exchange]
-- exchangesExample = do
--   conn <- getPsqlConnection commonFilePath
--   tups <- runQuery conn exchangeQuery
--   closePsqlConnection conn
--   return tups

-- insertExchange :: Exchange' a b c -> Connection -> IO Int64
-- insertExchange exchange connection =
--   runInsertMany connection exchangeTable [exchange]

-- nasdaq :: Exchange
-- nasdaq = Exchange' "NASDAQ" (TimeZone (-300) False "US/Eastern") (-300)

-- nyse :: Exchange
-- nyse = Exchange' "NYSE" (TimeZone (-300) False "US/Eastern") (-300)

-- exchanges = [nasdaq, nyse]

