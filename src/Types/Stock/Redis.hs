{-# LANGUAGE OverloadedStrings #-}

module Types.Stock.Redis where

import Control.Monad
import Data.Traversable
import Data.UUID
import Data.Time.Clock
import qualified Data.List as L

import qualified Data.ByteString.Char8 as BS

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
  in set (BS.pack ("mostRecentTick:"++(show uuid))) (BS.pack tickTimeStr)

getLatestTimestamp' stockId = let
  uuid :: UUID
  uuid = stockId -- clunky way to bound type of argument
  in get (BS.pack ("mostRecentTick:"++(show uuid)))

getLatestTimestamps :: Redis [(UUID, String)]
getLatestTimestamps = do
  -- TODO unsafe!
  (Right stockIds) <- keys "mostRecentTick:*"
  (Right timestamps) <- mget stockIds
  let stockIds' = (BS.drop 15) <$> stockIds
      (Just timestamps') = sequence timestamps
      timestamps'' = BS.unpack <$> timestamps'
      
      stockIds'' = (\bs -> let mUUID = (fromString . BS.unpack) bs
                               -- TODO make safe!
                           in case mUUID of
                                (Just uuid) -> uuid
                   ) <$> stockIds'
        
  return $ zip stockIds'' timestamps''

-- exampleTimestamps :: IO [(UUID, BS.ByteString)]
-- exampleTimestamps = do
--   conn <- getRedisConnection commonFilePath

--   tups <- runRedis conn getLatestTimestamps
  
--   closeRedisConnection conn

--   return tups

  


