{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Types.Tick.Psql where

import GHC.Generics

import Data.Monoid

import Data.Int (Int64)

import Data.Time.LocalTime
import Data.Time (UTCTime, getCurrentTime)

import Data.Text (Text)
import Data.Functor.Identity

import qualified Data.Text.Lazy as Text (pack)


import           Data.Profunctor.Product (p7)
import           Data.Profunctor.Product.Default (def)
import           Opaleye (Column, Table(Table),
                          required, optional, (.==), (.<),
                          PGInt4, PGFloat8)

import Opaleye.Manipulation

import qualified Opaleye.Table as T
import           Opaleye (Column)
import qualified Opaleye.PGTypes as P

import Database.PostgreSQL.Simple.Internal (Connection)

import Types.Tick
import Types.Stock

-- TODO foreign key

ticksTableStr :: String
ticksTableStr = "create table if not exists ticks"
  <> " ( time timestamp not null"
  -- <> ", symbol text not null"
  <> ", open float8 not null"
  <> ", high float8 not null"
  <> ", low float8 not null"
  <> ", close float8 not null"
  <> ", volume int4 not null"
  <> ", stockId uuid not null"
  <> ", primary key (time, stockId)"
  <> ");"

ticksTable :: Table
              ((Column P.PGTimestamptz
               , Column P.PGFloat8
               , Column P.PGFloat8
               , Column P.PGFloat8
               , Column P.PGFloat8
               , Column P.PGInt4
               , Column P.PGUuid))
              ((Column P.PGTimestamptz
               , Column P.PGFloat8
               , Column P.PGFloat8
               , Column P.PGFloat8
               , Column P.PGFloat8
               , Column P.PGInt4
               , Column P.PGUuid))
ticksTable = T.Table "ticks" (p7 ( required "time"
                                 , required "open"
                                 , required "high"
                                 , required "low"
                                 , required "close"
                                 , required "volume"
                                 , required "stockId"
                                 ))


tickToPostgres :: Tick
               ->  (Column P.PGTimestamptz
                   , Column P.PGFloat8
                   , Column P.PGFloat8
                   , Column P.PGFloat8
                   , Column P.PGFloat8
                   , Column P.PGInt4
                   , Column P.PGUuid)
tickToPostgres (Tick utc open high low close volume (Stock stockId _ _ _)) = let
  in (P.pgUTCTime utc
     , P.pgDouble open
     , P.pgDouble high
     , P.pgDouble low
     , P.pgDouble close
     , P.pgInt4 volume
     , P.pgUUID stockId
     )


insertTick :: Tick -> Connection -> IO Int64
insertTick tick connection =
  runInsert connection ticksTable (tickToPostgres tick)


insertTicks :: [Tick] -> Connection -> IO Int64
insertTicks ticks connection =
  runInsertMany connection ticksTable (tickToPostgres <$> ticks)
