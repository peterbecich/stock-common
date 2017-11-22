{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Tick.Psql where

import Prelude

import Control.Arrow (returnA)
import Control.Concurrent (threadDelay)
import Data.Functor
import Data.Functor.Identity
import Data.Int (Int64)
import Data.Monoid
import Data.Profunctor.Product (p7)
import Data.Profunctor.Product.Default (def)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.LocalTime
import Data.UUID
import Database.PostgreSQL.Simple.Internal (Connection)
import GHC.Generics
import Opaleye (Query, Column, Table(Table), required, optional, (.==), (.<), PGInt4, PGFloat8, restrict, runQuery)
import Opaleye.Manipulation
import Opaleye.Order
import qualified Data.Text.Lazy as Text (pack)
import qualified Opaleye.PGTypes as P
import qualified Opaleye.Table as T

import Types.Tick
import Types.Stock
import Types.Stock.Psql
import Types.Exchange
import Types.Exchange.Psql

import DB.Psql

ticksTableStr :: String
ticksTableStr = "create table if not exists ticks"
  <> " ( time timestamptz not null"
  <> ", open float8 not null"
  <> ", high float8 not null"
  <> ", low float8 not null"
  <> ", close float8 not null"
  <> ", volume int4 not null"
  <> ", stockId uuid not null references stocks (stockId)"
  <> ", primary key (time, stockId)"
  <> ");"


type TickColumn = Tick'
                  (Column P.PGTimestamptz)
                  (Column P.PGFloat8)
                  (Column P.PGFloat8)
                  (Column P.PGFloat8)
                  (Column P.PGFloat8)
                  (Column P.PGInt4)
                  (Column P.PGUuid)

type TickColumn2 = Tick'
                  (Column P.PGTimestamptz)
                  (Column P.PGFloat8)
                  (Column P.PGFloat8)
                  (Column P.PGFloat8)
                  (Column P.PGFloat8)
                  (Column P.PGInt4)
                  (Column StockColumn)

-- tick timestamp
-- open
-- high
-- low
-- close
-- volume
-- (stock id, stock symbol, stock description, exchange name)
type TickColumn3 = Tick'
                  (Column P.PGTimestamptz)
                  (Column P.PGFloat8)
                  (Column P.PGFloat8)
                  (Column P.PGFloat8)
                  (Column P.PGFloat8)
                  (Column P.PGInt4)
                  ((Column P.PGUuid), (Column P.PGText), (Column P.PGText), (Column P.PGText))

-- tick timestamp
-- open
-- high
-- low
-- close
-- volume
-- (stock id, stock symbol, stock description, (exchange name, exchange time zone, tz offset))
type TickColumn4 = Tick'
                  (Column P.PGTimestamptz)
                  (Column P.PGFloat8)
                  (Column P.PGFloat8)
                  (Column P.PGFloat8)
                  (Column P.PGFloat8)
                  (Column P.PGInt4)
                  ((Column P.PGUuid)
                  , (Column P.PGText)
                  , (Column P.PGText)
                  , ((Column P.PGText)
                    , (Column P.PGText)
                    , (Column P.PGInt4)
                    )
                  )

$(makeAdaptorAndInstance "pTick" ''Tick')

tickTable :: Table TickColumn TickColumn
tickTable = T.Table "ticks" (pTick Tick' { time = required "time"
                                          , open = required "open"
                                          , high = required "high"
                                          , low = required "low"
                                          , close = required "close"
                                          , volume = required "volume"
                                          , stock = required "stockid"
                                          })

tickColumnQuery :: Query TickColumn
tickColumnQuery = T.queryTable tickTable

applyStockAndExchange :: TickColumn4
                      -> (Tick'
                          (Column P.PGTimestamptz)
                          (Column P.PGFloat8)
                          (Column P.PGFloat8)
                          (Column P.PGFloat8)
                          (Column P.PGFloat8)
                          (Column P.PGInt4)
                          (Stock'
                           (Column P.PGUuid)
                           (Column P.PGText)
                           (Column P.PGText)
                           ExchangeColumn
                          )
                         )
applyStockAndExchange (Tick' tickTimeC tickOpenC tickHighC tickLowC tickCloseC tickVolumeC
                       (stockIdC, stockSymbolC, stockDescriptionC, exchangeC)
                      ) = let
  stock :: (Stock'
             (Column P.PGUuid)
             (Column P.PGText)
             (Column P.PGText)
             (Exchange' (Column P.PGText) (Column P.PGText) (Column P.PGInt4)))
  stock = applyExchange' (Stock' stockIdC stockSymbolC stockDescriptionC exchangeC)
  in Tick' tickTimeC tickOpenC tickHighC tickLowC tickCloseC tickVolumeC stock


tickQuery :: Query (Tick'
                   (Column P.PGTimestamptz)
                   (Column P.PGFloat8)
                   (Column P.PGFloat8)
                   (Column P.PGFloat8)
                   (Column P.PGFloat8)
                   (Column P.PGInt4)
                   (Stock' (Column P.PGUuid) (Column P.PGText) (Column P.PGText)
                    (Exchange' (Column P.PGText) (Column P.PGText) (Column P.PGInt4))
                   )
                   )
tickQuery = proc () -> do
  tickRow@(Tick' tickTimeC tickOpenC tickHighC tickLowC tickCloseC tickVolumeC stockIdC) <- tickColumnQuery -< ()
  stockRow@(Stock' stockIdC' symbolC descriptionC exchangeNameC) <- stockColumnQuery -< ()
  exchangeRow@(Exchange' exchangeNameC' exchangeTimeZoneC exchangeTimeZoneOffsetC) <- exchangeQuery -< ()
  restrict -< stockIdC .== stockIdC'
  restrict -< exchangeNameC .== exchangeNameC'

  let
    intermediate :: Tick'
                   (Column P.PGTimestamptz)
                   (Column P.PGFloat8)
                   (Column P.PGFloat8)
                   (Column P.PGFloat8)
                   (Column P.PGFloat8)
                   (Column P.PGInt4)
                   ((Column P.PGUuid), (Column P.PGText), (Column P.PGText),
                    ((Column P.PGText), (Column P.PGText), (Column P.PGInt4))
                   )
    intermediate = Tick' tickTimeC tickOpenC tickHighC tickLowC tickCloseC tickVolumeC (stockIdC, symbolC, descriptionC, (exchangeNameC, exchangeTimeZoneC, exchangeTimeZoneOffsetC))

  returnA -< applyStockAndExchange intermediate
  

recentTickQuery :: Query (Tick'
                   (Column P.PGTimestamptz)
                   (Column P.PGFloat8)
                   (Column P.PGFloat8)
                   (Column P.PGFloat8)
                   (Column P.PGFloat8)
                   (Column P.PGInt4)
                   (Stock' (Column P.PGUuid) (Column P.PGText) (Column P.PGText)
                    (Exchange' (Column P.PGText) (Column P.PGText) (Column P.PGInt4))
                   )
                   )

tickExample :: IO [Tick]
tickExample = do
  conn <- getPsqlConnection commonFilePath
  ticks <- runQuery conn tickQuery
  closePsqlConnection conn
  return (take 10 ticks)
                   
printTicks = tickExample >>= mapM_ (putStrLn . show)

recentTickQuery = orderBy (desc (\(Tick' ts _ _ _ _ _ _) -> ts)) tickQuery

recentTickExample :: IO [Tick]
recentTickExample = do
  conn <- getPsqlConnection commonFilePath
  ticks <- runQuery conn recentTickQuery
  closePsqlConnection conn
  return (take 100 ticks)
                   
printRecentTicks = recentTickExample >>= mapM_ (putStrLn . show)


tickToPostgres :: Tick -> TickColumn
tickToPostgres (Tick' utc open high low close volume (Stock' stockId _ _ _)) = let
  in Tick'
     (P.pgUTCTime utc)
     (P.pgDouble open)
     (P.pgDouble high)
     (P.pgDouble low)
     (P.pgDouble close)
     (P.pgInt4 volume)
     (P.pgUUID stockId)

insertTick :: Tick -> Connection -> IO Int64
insertTick tick connection =
  runInsert connection tickTable (tickToPostgres tick)

insertTicks :: [Tick] -> Connection -> IO Int64
insertTicks ticks connection =
  runInsertMany connection tickTable (tickToPostgres <$> ticks)

-- Eventual solution to inserting duplicate ticks
-- https://www.postgresql.org/docs/current/static/sql-insert.html
-- https://github.com/tomjaguarpaw/haskell-opaleye/issues/139

-- bogusUUID :: UUID
-- (Just bogusUUID) = fromString "6500a7a7-b839-4591-9c79-f908d6c46386"

