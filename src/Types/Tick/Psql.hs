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
import Data.List
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

type TickColumn5 = Tick'
                   (Column P.PGTimestamptz)
                   (Column P.PGFloat8)
                   (Column P.PGFloat8)
                   (Column P.PGFloat8)
                   (Column P.PGFloat8)
                   (Column P.PGInt4)
                   StockColumn4

type PriceColumn = Column P.PGFloat8

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
                      -> TickColumn5
applyStockAndExchange (Tick' tickTimeC tickOpenC tickHighC tickLowC tickCloseC tickVolumeC
                       (stockIdC, stockSymbolC, stockDescriptionC, exchangeC)
                      ) = let
  stock :: StockColumn4
  stock = applyExchange' (Stock' stockIdC stockSymbolC stockDescriptionC exchangeC)
  in Tick' tickTimeC tickOpenC tickHighC tickLowC tickCloseC tickVolumeC stock


-- all ticks
tickQuery :: Query TickColumn5
tickQuery = proc () -> do
  tickRow@(Tick' tickTimeC tickOpenC tickHighC tickLowC tickCloseC tickVolumeC stockIdC) <- tickColumnQuery -< ()
  stockRow@(Stock' stockIdC' symbolC descriptionC exchangeNameC) <- stockColumnQuery -< ()
  exchangeRow@(Exchange' exchangeNameC' exchangeTimeZoneC exchangeTimeZoneOffsetC) <- exchangeQuery -< ()
  restrict -< stockIdC .== stockIdC'
  restrict -< exchangeNameC .== exchangeNameC'

  let
    intermediate :: TickColumn4
    intermediate = Tick' tickTimeC tickOpenC tickHighC tickLowC tickCloseC tickVolumeC (stockIdC, symbolC, descriptionC, (exchangeNameC, exchangeTimeZoneC, exchangeTimeZoneOffsetC))

  returnA -< applyStockAndExchange intermediate

-- all ticks, ordered by time descending
recentTickQuery :: Query TickColumn5
recentTickQuery = orderBy (desc (\(Tick' ts _ _ _ _ _ _) -> ts)) tickQuery

-- all ticks for a given stock
stockTicksQuery' :: UUID -> Query TickColumn5
stockTicksQuery' stockId = proc () -> do
  tickRow@(Tick' tickTimeC tickOpenC tickHighC tickLowC tickCloseC tickVolumeC (Stock' stockIdC _ _ _)) <- tickQuery -< ()
  let sid = P.pgUUID stockId
  restrict -< sid .== stockIdC

  returnA -< tickRow

-- all ticks for a given stock
stockTicksQuery (Stock' stockId _ _ _) = stockTicksQuery' stockId


-- stockTickCountQuery :: UUID -> Query Int
-- stockTickCountQuery stockId = length <$> (stockTicksQuery' stockId)

-- all closing prices for a given stock
stockCloseQuery' :: UUID -> Query PriceColumn
stockCloseQuery' stockId = proc () -> do
  (Tick' ts _ _ _ tickCloseC _ (Stock' stockIdC _ _ _)) <- orderBy (desc (\(Tick' ts _ _ _ _ _ _) -> ts)) tickQuery -< ()
  let sid = P.pgUUID stockId
  restrict -< sid .== stockIdC

  returnA -< tickCloseC

-- all closing prices for a given stock
stockCloseQuery (Stock' stockId _ _ _) = stockCloseQuery' stockId

getStockClose :: PostgresPool -> UUID -> IO [Double]
getStockClose pool stockId =
  runQueryPool pool (stockCloseQuery' stockId)

-- all ticks that match a given stock ID and timestamp
stockTickQuery' :: UUID -> UTCTime -> Query TickColumn5
stockTickQuery' stockId timestamp = proc () -> do
  tickRow@(Tick' tickTimeC tickOpenC tickHighC tickLowC tickCloseC tickVolumeC (Stock' stockIdC _ _ _)) <- tickQuery -< ()
  let sid = P.pgUUID stockId
  restrict -< sid .== stockIdC
  let ts = P.pgUTCTime timestamp
  restrict -< ts .== tickTimeC
  returnA -< tickRow

stockTickQuery (Tick' ts _ _ _ _ _ (Stock' stockId _ _ _)) =
  stockTickQuery' stockId ts

-- most recent ticks for a given stock
recentStockTicksQuery' :: UUID -> Query TickColumn5
recentStockTicksQuery' stockId =
  orderBy (desc (\(Tick' ts _ _ _ _ _ _) -> ts)) (stockTicksQuery' stockId)

recentStockTicksQuery (Stock' stockId _ _ _) = recentStockTicksQuery' stockId

-- most recent single tick
recentStockTickQuery' stockId =
  limit 1 (recentStockTicksQuery' stockId)

recentStockTickQuery (Stock' stockId _ _ _) =
  recentStockTicksQuery' stockId

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

-- entire transaction fails when duplicate tick inserted

insertTick :: PostgresPool -> Tick -> IO Int64
insertTick pool tick =
  runInsertPool pool tickTable [(tickToPostgres tick)]

insertTicks :: PostgresPool -> [Tick] -> IO Int64
insertTicks pool ticks =
  runInsertPool pool tickTable (tickToPostgres <$> ticks)

-- Eventual solution to inserting duplicate ticks
-- https://www.postgresql.org/docs/current/static/sql-insert.html
-- https://github.com/tomjaguarpaw/haskell-opaleye/issues/139

-- TODO implement this within single call to `runQuery`??
-- This implementation only inserts ticks with timestamps more recent
-- than ticks already in the table, for a given stock
-- insertTicksSafe :: [Tick] -> Connection -> IO Int64
-- insertTicksSafe ticks@(t:ts) connection = do
--   recentTicks <- runQuery connection (recentStockTickQuery (stock t)) :: IO [Tick]
--   case recentTicks of
--     (recentTick:recentTicks') ->
--       let newerTicks = filter (\tick -> (time tick) > (time recentTick)) ticks
--       in runInsertMany connection tickTable (tickToPostgres <$> newerTicks)      
--     _ -> runInsertMany connection tickTable (tickToPostgres <$> ticks)      

insertTicksSafe :: PostgresPool -> [Tick] -> IO Int64
insertTicksSafe pool ticks = sum <$> mapM f ticks
  where
    f :: Tick -> IO Int64
    f tick = do
      ticks <- runQueryPool pool (stockTickQuery tick)
      let
        mDuplicateTick :: Maybe Tick
        mDuplicateTick = find (\tick' -> (stockId (stock tick')) == (stockId (stock tick))
                                         && (time tick') == (time tick)
                              ) ticks
      case mDuplicateTick of
        (Just duplicateTick) -> return 0 -- tick already in DB
        (Nothing) -> insertTick pool tick
  




