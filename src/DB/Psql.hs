{-# LANGUAGE OverloadedStrings #-}

module DB.Psql where

import Database.PostgreSQL.Simple

import qualified Data.Yaml.Config as Config

getPsqlConnection :: IO Connection
getPsqlConnection = do
  let
    path :: FilePath
    path = "conf/collector.yaml"
  config <- Config.load path
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
