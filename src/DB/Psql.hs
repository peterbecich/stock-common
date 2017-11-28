{-# LANGUAGE OverloadedStrings #-}

module DB.Psql where

import Control.Monad (return)
import Opaleye.RunQuery (runQuery)
import Database.PostgreSQL.Simple
import qualified Data.Yaml.Config as Config
import qualified Opaleye.Internal.QueryArr as OE (Query)

commonFilePath :: FilePath
commonFilePath = "conf/common.yaml"

getPsqlConnection :: FilePath -> IO Connection
getPsqlConnection filePath = do
  config <- Config.load filePath
  db <- Config.subconfig "db" config
  postgres <- Config.subconfig "postgres" db
  dbname <- Config.lookup "dbname" postgres
  user <- Config.lookup "user" postgres
  password <- Config.lookup "password" postgres
  ip <- Config.lookup "ip" postgres
  port <- Config.lookup "port" postgres
  let
    connInfo :: ConnectInfo
    connInfo = ConnectInfo ip port user password dbname
  connect connInfo

closePsqlConnection :: Connection -> IO ()
closePsqlConnection conn = close conn


-- wastefully create and close Postgres connection for every call
-- don't attempt to wrap runQuery like this;
-- type parameters are not resolved
-- https://www.stackage.org/haddock/lts-9.10/opaleye-0.5.4.0/Opaleye-RunQuery.html#v:runQuery
-- getPsqlConnRun :: (Connection -> IO a) -> IO a
-- getPsqlConnRun f = do
--   psqlConn <- getPsqlConnection commonFilePath
--   a <- f psqlConn
--   closePsqlConnection psqlConn
--   return a

-- getPsqlConnRunQuery :: (OE.Query a) -> IO [a]
-- getPsqlConnRunQuery query = do
--   psqlConn <- getPsqlConnection commonFilePath
--   x <- runQuery psqlConn query
--   closePsqlConnection psqlConn
--   return x
  

