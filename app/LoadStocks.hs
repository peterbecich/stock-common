
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}


module Main where

import DB.Psql
import Types.Exchange.Psql
import Util.LoadStocks
-- import qualified Data.Pool as Pool

main :: IO ()
main = do
  putStrLn "load exchanges and stocks"

  psqlPool <- createPostgresPool commonFilePath

  exchangeCount <- runInsertPool psqlPool exchangeTable (exchangeToPsql <$> exchanges)

  putStrLn $ "inserted " ++ show exchangeCount ++ " exchanges"

  insertSymbols psqlPool symbolFiles
  
  
  
