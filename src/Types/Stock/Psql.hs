{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Types.Stock.Psql where

import Prelude

import Data.Profunctor.Product (p4)
import Data.Profunctor.Product.Default (def)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Opaleye (Query, Column, Table(Table), required, optional, (.==), (.<), PGInt4, PGFloat8, runQuery, restrict)
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

-- TODO foreign key
stocksTableStr :: String
stocksTableStr = "create table if not exists stocks"
       <> " ( stockId uuid not null primary key"
       <> " , symbol text not null unique"
       <> " , description text not null"
       <> " , exchange text not null references exchanges (name)"
       <> " );"

type StockColumn = Stock' (Column P.PGUuid) (Column P.PGText) (Column P.PGText) (Column P.PGText)

$(makeAdaptorAndInstance "pStock" ''Stock')

stockTable :: Table StockColumn StockColumn
stockTable = T.Table "stocks" (pStock Stock' { stockId = required "stockId"
                                              , symbol = required "symbol"
                                              , description = required "description"
                                              , exchange = required "exchange"
                                              })

stockQuery :: Query StockColumn
stockQuery = T.queryTable stockTable

-- stockExample :: IO [Stock]
-- stockExample = do
--   conn <- getPsqlConnection commonFilePath
--   stocks <- runQuery conn stockQuery
--   closePsqlConnection conn
--   return stocks

stockToPsql :: Stock -> StockColumn
stockToPsql stock = Stock'
                    (P.pgUUID (stockId stock))
                    (P.pgString (symbol stock))
                    (P.pgString (description stock))
                    (P.pgString (name (exchange stock)))

-- insertStock :: Stock -> Connection -> IO Int64
-- insertStock stock connection =
--   runInsert connection stocksTable (stockToPsql stock)

-- insertStocks :: [Stock] -> Connection -> IO Int64
-- insertStocks stocks connection =
--   runInsertMany connection stocksTable (stockToPsql <$> stocks)

-- getCStocks :: Query (Column P.PGUuid, Column P.PGText, Column P.PGText, Column P.PGText)
-- getCStocks = T.queryTable stocksTable

-- -- TODO don't hardcode NASDAQ!
-- getStocks :: Connection -> IO [Stock]
-- getStocks conn = do
--   tups <- runQuery conn getCStocks :: IO [(UUID, String, String, String)]
--   let stocks = (\(uid, sym, dsp, _) -> Stock uid sym dsp nasdaq) <$> tups
--   return stocks

-- -- https://github.com/tomjaguarpaw/haskell-opaleye/blob/master/Doc/Tutorial/TutorialBasic.lhs#L259
-- getCStock :: UUID -> Query (Column P.PGUuid, Column P.PGText, Column P.PGText, Column P.PGText)
-- getCStock uuid = proc () -> do
--   row@(uuid', _, _, _) <- getCStocks -< ()
--   restrict -< uuid' .== (P.pgUUID uuid)

--   returnA -< row
                  

-- getStocksExample :: IO [Stock]
-- getStocksExample = do
--   conn <- getPsqlConnection commonFilePath
--   stocks <- getStocks conn
--   -- mapM_ (putStrLn . show)  (take 16 stocks)
--   closePsqlConnection conn
--   return stocks

-- getNStocksExample :: Int -> IO [Stock]
-- getNStocksExample n = do
--   conn <- getPsqlConnection commonFilePath
--   stocks <- getStocks conn
--   -- mapM_ (putStrLn . show)  (take 16 stocks)
--   closePsqlConnection conn
--   return (take n stocks)

  
-- -- TODO more selective queries
-- -- see https://github.com/tomjaguarpaw/haskell-opaleye/blob/master/Doc/Tutorial/TutorialBasic.lhs#L461
-- -- https://github.com/tomjaguarpaw/haskell-opaleye/blob/master/Doc/Tutorial/TutorialBasic.lhs#L559


-- -- TODO make safe!!
-- -- TODO don't hardcode nasdaq!
-- getStock :: UUID -> Connection -> IO [Stock]
-- getStock uuid conn = do
--   stock <- runQuery conn (getCStock uuid) :: IO [(UUID, String, String, String)]
--   let stock' = (\(stockId, symbol, description, _) -> Stock stockId symbol description nasdaq) <$> stock
--   return stock'

-- bogusUUID :: UUID
-- (Just bogusUUID) = fromString "cb962035-1394-4f87-92e2-d8e57072b185"

-- getStockExample :: UUID -> IO [Stock]
-- getStockExample uuid = do
--   conn <- getPsqlConnection commonFilePath
--   stock <- getStock uuid conn
--   -- mapM_ (putStrLn . show)  (take 16 stocks)
--   closePsqlConnection conn
--   return stock

