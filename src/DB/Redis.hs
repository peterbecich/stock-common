{-# LANGUAGE OverloadedStrings #-}

module DB.Redis where

import qualified Network.Socket as Sock (PortNumber)
import Database.Redis
import qualified Data.Yaml.Config as Config

-- TODO store this constant in one place
commonFilePath :: FilePath
commonFilePath = "conf/common.yaml"

getRedisConnection :: FilePath -> IO Connection
getRedisConnection filePath = do
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
    connInfo = defaultConnectInfo
        { connectHost = ip
        -- , connectPort = PortNumber port
        -- , connectPort = 6379
        }
  -- connect connInfo
  checkedConnect connInfo

-- probably anti-pattern...
--closeRedisConnection :: Connection ->
closeRedisConnection conn = do
  runRedis conn quit
  
