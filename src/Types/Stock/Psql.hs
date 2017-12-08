{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Types.Stock.Psql where

import Prelude

import Control.Arrow (returnA)
import Control.Monad
import Data.Char
import Data.Functor ((<$>), fmap)
import Data.Int (Int64)
import Data.Map (Map, empty, size, mapKeys, toList, assocs)
import Data.Monoid
import Data.Profunctor.Product (p4)
import Data.Profunctor.Product.Default (def)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Time.LocalTime
import Data.UUID
import Database.PostgreSQL.Simple.Internal (Connection)
import Opaleye (Query, Column, Table(Table), required, optional, (.==), (.<), PGInt4, PGFloat8, runQuery, restrict)
import Opaleye.Internal.RunQuery
import Opaleye.Manipulation
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Opaleye.Constant as C
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.PGTypes as P
import qualified Opaleye.Table as T

import Types.Exchange.Psql (nasdaq)

import Types.Stock
import Types.Exchange
import Types.Exchange.Psql


import DB.Psql

stocksTableStr :: String
stocksTableStr = "create table if not exists stocks"
       <> " ( stockId uuid not null primary key"
       <> " , symbol text not null unique"
       <> " , description text not null"
       <> " , exchange text not null references exchanges (name)"
       <> " );"

type StockColumn = Stock' (Column P.PGUuid) (Column P.PGText) (Column P.PGText) (Column P.PGText)

-- stock id, stock symbol, stock description, (exchange name, exchange timezone, exchange offset)
type StockColumn3 = Stock'
                    (Column P.PGUuid)
                    (Column P.PGText)
                    (Column P.PGText)
                    ((Column P.PGText), (Column P.PGText), (Column P.PGInt4))

type StockColumn4 = (Stock'
                     (Column P.PGUuid)
                     (Column P.PGText)
                     (Column P.PGText)
                     ExchangeColumn
                    )

$(makeAdaptorAndInstance "pStock" ''Stock')

stockTable :: Table StockColumn StockColumn
stockTable = T.Table "stocks" (pStock Stock' { stockId = required "stockid"
                                             , symbol = required "symbol"
                                             , description = required "description"
                                             , exchange = required "exchange"
                                             })

stockColumnQuery :: Query StockColumn
stockColumnQuery = T.queryTable stockTable


applyExchange' :: StockColumn3
               -> StockColumn4
applyExchange' (Stock' stockIdC stockSymbolC stockDescriptionC (exchangeNameC, exchangeTimeZoneC, exchangeTimeZoneOffsetC)) =
  Stock' stockIdC stockSymbolC stockDescriptionC (Exchange' exchangeNameC exchangeTimeZoneC exchangeTimeZoneOffsetC)

-- all stocks
stockQuery :: Query StockColumn4
stockQuery = proc () -> do
  stockRow@(Stock' stockIdC symbolC descriptionC exchangeNameC) <- stockColumnQuery -< ()
  exchangeRow@(Exchange' exchangeNameC' exchangeTimeZoneC exchangeTimeZoneOffsetC) <- exchangeQuery -< ()
  restrict -< exchangeNameC' .== exchangeNameC
  let intermediate = Stock' stockIdC symbolC descriptionC (exchangeNameC, exchangeTimeZoneC, exchangeTimeZoneOffsetC)
  
  returnA -< applyExchange' intermediate

stockIdQuery :: UUID -> Query StockColumn4
stockIdQuery stockId = proc () -> do
  stockRow@(Stock' stockIdC symbolC descriptionC exchangeNameC) <- stockColumnQuery -< ()
  restrict -< stockIdC .== (P.pgUUID stockId)
  let ex = exchange stockRow
  exchangeRow@(Exchange' exchangeNameC' exchangeTimeZoneC exchangeTimeZoneOffsetC) <- exchangeQuery -< ()
  restrict -< exchangeNameC' .== exchangeNameC
  let intermediate = Stock' stockIdC symbolC descriptionC (exchangeNameC, exchangeTimeZoneC, exchangeTimeZoneOffsetC)
  
  returnA -< applyExchange' intermediate

-- see TutorialBasic.lhs
-- stocksWithTicksQuery :: Query StockColumn4
-- stocksWithTicksQuery = proc () -> do
--   stock <- stockQuery -< ()
  -- restrict -< undefined

getStock :: UUID -> Connection -> IO [Stock]
getStock stockId conn = runQuery conn (stockIdQuery stockId)

getStocks :: Connection -> IO [Stock]
getStocks conn = runQuery conn stockQuery

stockToPsql :: Stock -> StockColumn
stockToPsql stock = Stock'
                    (P.pgUUID (stockId stock))
                    (P.pgString (symbol stock))
                    (P.pgString (description stock))
                    (P.pgString (name (exchange stock)))

insertStock :: Stock -> Connection -> IO Int64
insertStock stock connection =
  runInsert connection stockTable (stockToPsql stock)

insertStocks :: [Stock] -> Connection -> IO Int64
insertStocks stocks connection =
  runInsertMany connection stockTable (stockToPsql <$> stocks)

-- -- TODO more selective queries
-- -- see https://github.com/tomjaguarpaw/haskell-opaleye/blob/master/Doc/Tutorial/TutorialBasic.lhs#L461
-- -- https://github.com/tomjaguarpaw/haskell-opaleye/blob/master/Doc/Tutorial/TutorialBasic.lhs#L559


