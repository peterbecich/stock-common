{-# LANGUAGE OverloadedStrings #-}

module DB.Psql where

import Database.PostgreSQL.Simple

import qualified Data.Yaml.Config as Config

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
