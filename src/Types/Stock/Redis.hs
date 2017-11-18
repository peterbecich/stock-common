{-# LANGUAGE OverloadedStrings #-}

module Types.Stock.Redis where

import Data.Time.Clock

import Data.ByteString.Char8

import Database.Redis

import Types.Stock
import qualified Types.Tick as Tick


-- set most recent tick
-- get most recent tick

-- TODO move to Tick module ?
setTickTimestamp tick = let
  tickTimeStr :: String
  tickTimeStr = show (Tick.time tick)
  sym :: String
  sym = symbol $ Tick.stock tick
  in set (pack ("mostRecentTick:"++sym)) (pack tickTimeStr)

getLatestTimestamp stock = let
  sym :: String
  sym = symbol stock
  in get (pack ("mostRecentTick:"++sym))


