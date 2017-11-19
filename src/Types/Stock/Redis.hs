{-# LANGUAGE OverloadedStrings #-}

module Types.Stock.Redis where

import Control.Monad
import Data.Traversable
import Data.UUID
import Data.Time.Clock

import Data.ByteString.Char8

import Database.Redis

import DB.Redis
import Types.Stock
import qualified Types.Tick as Tick


-- set most recent tick
-- get most recent tick

-- TODO move to Tick module ?
setTickTimestamp tick = let
  tickTimeStr :: String
  tickTimeStr = show (Tick.time tick)
  -- sym :: String
  -- sym = symbol $ Tick.stock tick
  uuid = stockId (Tick.stock tick)
  in set (pack ("mostRecentTick:"++(show uuid))) (pack tickTimeStr)

-- getLatestTimestamp :: Stock -> m (f (Maybe ByteString))
getLatestTimestamp stock = let
  -- sym :: String
  -- sym = symbol stock
  uuid = stockId stock
  in get (pack ("mostRecentTick:"++(show uuid)))

--getLatestTimestamp' :: UUID -> m (f (Maybe ByteString))
getLatestTimestamp' stockId = let
  -- sym :: String
  -- sym = symbol stock
  uuid :: UUID
  uuid = stockId -- clunky way to bound type of argument
  in get (pack ("mostRecentTick:"++(show uuid)))

getLatestTimestamps stocks = 
  traverse getLatestTimestamp stocks


exampleTimestamps stocks = do
  conn <- getRedisConnection commonFilePath
  timestamps <- runRedis conn (getLatestTimestamps stocks)
  closeRedisConnection conn
  return timestamps

  


