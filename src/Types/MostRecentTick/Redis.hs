{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Types.MostRecentTick.Redis where

import Control.Monad
import Data.Time.Clock
import Data.Traversable
import Data.UUID
import Database.Redis
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Map.Lazy as Map

import DB.Redis
import qualified Types.Stock as Stock
import qualified Types.Tick as Tick
import qualified Types.MostRecentTick as MRT

setTickTimestamp tick = let
  tickTimeStr :: String
  tickTimeStr = show (Tick.time tick)
  uuid :: UUID
  uuid = Stock.stockId (Tick.stock tick)
  in set (BS.pack ("mostRecentTick:"++(show uuid))) (BS.pack tickTimeStr)

-- TODO improve!!
--getLatestTimestamp :: (RedisCtx m f) => UUID -> m (f (Maybe BS.ByteString))
getLatestTimestamp :: UUID -> Redis (Either Reply (Maybe UTCTime))
getLatestTimestamp stockId = do
  -- unsafe!!
  (Right mBS) <- get (BS.pack ("mostRecentTick:"++(show stockId)))
  let
    -- unsafe!!
    bs :: BS.ByteString
    (Just bs) = mBS

    s :: String
    s = BS.unpack bs
    
    -- unsafe!!
    utc :: UTCTime
    utc = read s

  return $ Right $ Just utc
    
-- exampleTimestamp :: IO (Either MRT.MostRecentTick)
exampleTimestamp = do
  conn <- getRedisConnection commonFilePath
  let
    bogusUUID :: UUID
    (Just bogusUUID) = fromString "cce815fa-08f5-4ca1-bbc8-6766c7a5d430"

  mrt <- runRedis conn (getLatestTimestamp bogusUUID)
  
  closeRedisConnection conn

  return mrt


getLatestTimestamps :: Redis (Map.Map UUID UTCTime)
getLatestTimestamps = do
  -- TODO unsafe!
  (Right stockIds) <- keys "mostRecentTick:*"
  (Right timestamps) <- mget stockIds
  let
    stockIds' :: [BS.ByteString]
    stockIds' = (BS.drop 15) <$> stockIds

    stockIds'' :: [UUID]
    stockIds'' = (\bs -> let mUUID = (fromString . BS.unpack) bs
                   -- TODO make safe!
                         in case mUUID of
                           (Just uuid) -> uuid
                 ) <$> stockIds'
    

    timestamps' :: [BS.ByteString]
    (Just timestamps') = sequence timestamps

    timestamps'' :: [String]
    timestamps'' = BS.unpack <$> timestamps'

    timestamps''' :: [UTCTime]
    timestamps''' = read <$> timestamps''

    tups :: [(UUID, UTCTime)]
    tups = (zip stockIds'' timestamps''')

    
        
  return $ Map.fromList tups

exampleTimestamps :: IO (Map.Map UUID UTCTime)
exampleTimestamps = do
  conn <- getRedisConnection commonFilePath

  tups <- runRedis conn getLatestTimestamps
  
  closeRedisConnection conn

  return tups

  


