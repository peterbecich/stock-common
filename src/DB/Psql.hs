{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module DB.Psql where

import Control.Monad (return)
import Data.Int (Int64)
import Data.Profunctor.Product.Default
import Opaleye.Manipulation (runInsertMany)
import Opaleye.RunQuery (runQuery)
import Opaleye.Internal.RunQuery (QueryRunner)
import Opaleye.Internal.QueryArr (Query)
import Opaleye.Internal.Table
import System.Environment (getEnv)
import qualified Data.Pool as Pool
import qualified Data.Yaml.Config as Config
import qualified Database.PostgreSQL.Simple as PSQL
import qualified Opaleye.Internal.QueryArr as OE (Query)

commonFilePath :: FilePath
commonFilePath = "/usr/local/etc/common.yaml"

getConnInfo :: FilePath -> IO PSQL.ConnectInfo
getConnInfo filePath = do
  config <- Config.load filePath
  db <- Config.subconfig "db" config
  postgres <- Config.subconfig "postgres" db
  dbname <- Config.lookup "dbname" postgres
  user <- Config.lookup "user" postgres
  -- password <- Config.lookup "password" postgres
  password <- getPsqlPassword
  ip <- Config.lookup "ip" postgres
  port <- Config.lookup "port" postgres
  let
    connInfo :: PSQL.ConnectInfo
    connInfo = PSQL.ConnectInfo ip port user password dbname
  return connInfo

-- https://hackage.haskell.org/package/base-4.10.0.0/docs/System-Environment.html
getPsqlPassword :: IO String
getPsqlPassword = getEnv "POSTGRES_PASSWORD"

getPsqlConnection :: FilePath -> IO PSQL.Connection
getPsqlConnection filePath = do
  config <- Config.load filePath
  db <- Config.subconfig "db" config
  postgres <- Config.subconfig "postgres" db
  dbname <- Config.lookup "dbname" postgres
  user <- Config.lookup "user" postgres
  --password <- Config.lookup "password" postgres
  password <- getPsqlPassword
  ip <- Config.lookup "ip" postgres
  port <- Config.lookup "port" postgres
  let
    connInfo :: PSQL.ConnectInfo
    connInfo = PSQL.ConnectInfo ip port user password dbname
  PSQL.connect connInfo

closePsqlConnection :: PSQL.Connection -> IO ()
closePsqlConnection conn = PSQL.close conn

-- https://gist.github.com/thoughtpolice/e89d370f91039774f98bccf21e0cd877
-- https://hackage.haskell.org/package/resource-pool-0.2.3.2/docs/Data-Pool.html
-- https://www.stackage.org/haddock/lts-9.10/opaleye-0.5.4.0/Opaleye-RunQuery.html#v:runQuery

type PostgresPool = Pool.Pool PSQL.Connection

createPostgresPool :: FilePath
                   -> IO PostgresPool
createPostgresPool filePath = do
  connInfo <- getConnInfo filePath
  Pool.createPool (PSQL.connect connInfo) PSQL.close 4 1 4
  
  
runQueryPool :: Default QueryRunner cols vals
             => PostgresPool
             -> Query cols
             -> IO [vals]
runQueryPool pool query = Pool.withResource pool (\conn -> runQuery conn query)

runInsertPool :: PostgresPool
              -> Table columns columns'
              -> [columns]
              -> IO Int64
runInsertPool pool table rows =
  Pool.withResource pool (\conn -> runInsertMany conn table rows)

  
