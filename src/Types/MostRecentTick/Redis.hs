{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Types.MostRecentTick.Redis where

import Control.Monad
import Data.Traversable
import Data.UUID
import Data.Time.Clock
import qualified Data.List as L

import qualified Data.ByteString.Char8 as BS

import Database.Redis

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

--getLatestTimestamp :: (RedisCtx m f) => UUID -> m (f (Maybe BS.ByteString))
getLatestTimestamp :: UUID -> Redis (Either Reply (Maybe BS.ByteString))
getLatestTimestamp stockId =
  get (BS.pack ("mostRecentTick:"++(show stockId)))

-- exampleTimestamp :: IO (Either MRT.MostRecentTick)
exampleTimestamp = do
  conn <- getRedisConnection commonFilePath
  let
    bogusUUID :: UUID
    (Just bogusUUID) = fromString "cce815fa-08f5-4ca1-bbc8-6766c7a5d430"

  mrt <- runRedis conn (getLatestTimestamp bogusUUID)
  
  closeRedisConnection conn

  return mrt
  
getLatestTimestamp' :: UUID -> Redis (Either Reply (Maybe MRT.MostRecentTick))
getLatestTimestamp' stockId = do
  eitherMaybeByteString <- getLatestTimestamp stockId
  let
    --eitherMString :: (f (Maybe String))
    eitherMString = (fmap . fmap) (BS.unpack) eitherMaybeByteString
    
    --eitherMUTC :: (f (Maybe UTCTime)) -- runtime error here if read fails
    eitherMUTC = (fmap . fmap) (read) eitherMString

    --eitherMMostRecentTick :: (f (Maybe MRT.MostRecentTick))
    eitherMMostRecentTick = (fmap . fmap) (\utc -> MRT.MostRecentTick stockId utc) eitherMUTC
    
  return eitherMMostRecentTick


-- TODO Improve
getLatestTimestamps :: Redis [MRT.MostRecentTick]
getLatestTimestamps = do
  -- TODO unsafe!
  (Right stockIds) <- keys "mostRecentTick:*"
  (Right timestamps) <- mget stockIds
  let stockIds' = (BS.drop 15) <$> stockIds
      (Just timestamps') = sequence timestamps
      timestamps'' = BS.unpack <$> timestamps'
      timestamps''' :: [UTCTime]
      timestamps''' = read <$> timestamps''
      
      stockIds'' = (\bs -> let mUUID = (fromString . BS.unpack) bs
                               -- TODO make safe!
                           in case mUUID of
                                (Just uuid) -> uuid
                   ) <$> stockIds'
      mrts = (uncurry MRT.MostRecentTick) <$> (zip stockIds'' timestamps''')
        
  return mrts

exampleTimestamps :: IO [MRT.MostRecentTick]
exampleTimestamps = do
  conn <- getRedisConnection commonFilePath

  tups <- runRedis conn getLatestTimestamps
  
  closeRedisConnection conn

  return tups

  


