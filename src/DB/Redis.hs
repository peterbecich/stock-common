{-# LANGUAGE OverloadedStrings #-}

module DB.Redis where

import Data.Functor
import System.Environment (getEnv)
import qualified Network.Socket as Sock (PortNumber)
import qualified Data.Pool as Pool
import qualified Database.Redis as Redis
import qualified Data.Yaml.Config as Config

-- TODO store this constant in one place
commonFilePath :: FilePath
commonFilePath = "conf/common.yaml"

-- https://hackage.haskell.org/package/base-4.10.0.0/docs/System-Environment.html
getRedisPassword :: IO String
getRedisPassword = getEnv "REDIS_PASSWORD"

getRedisConnectInfo :: FilePath -> IO Redis.ConnectInfo
getRedisConnectInfo filePath = do
  config <- Config.load filePath
  db <- Config.subconfig "db" config
  redis <- Config.subconfig "redis" db
  ip <- Config.lookup "ip" redis
  -- portStr <- Config.lookup "port" redis
  let
    -- Socket.PortNumber distinct from
    -- Hedis' PortNumber!
    -- TODO read port from config file
    -- port :: Sock.PortNumber
   -- port = read portStr
    connInfo = Redis.defaultConnectInfo
        { Redis.connectHost = ip
        -- , connectPort = PortNumber port
        -- , connectPort = 6379
        }
  return connInfo

  
getRedisConnection :: FilePath -> IO Redis.Connection
getRedisConnection filePath =
  (getRedisConnectInfo filePath) >>= Redis.checkedConnect

-- probably anti-pattern...
--closeRedisConnection :: Connection ->
closeRedisConnection conn = do
  Redis.runRedis conn Redis.quit
  
type RedisPool = Pool.Pool Redis.Connection

createRedisPool :: FilePath
                -> IO RedisPool
createRedisPool filePath = do
  connInfo <- getRedisConnectInfo filePath
  Pool.createPool
    (Redis.checkedConnect connInfo)
    (\conn -> void $ Redis.runRedis conn Redis.quit) -- TODO handle error
    4 1 4

runRedisPool :: RedisPool
             -> Redis.Redis a
             -> IO a
runRedisPool pool redis =
  Pool.withResource pool (\conn -> Redis.runRedis conn redis)

