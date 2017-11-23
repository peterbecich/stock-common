{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.LoadStocks where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V

import System.Random
import Data.UUID

import Control.Monad

import Data.Int (Int64)

import Types.Stock
import Types.Exchange
import Types.Exchange.Psql (nasdaq, nyse, amex)
import Types.Stock.Psql (insertStocks, insertStock)

import DB.Psql

nasdaqCSV = "data/nasdaq.csv"

-- nasdaqStr :: IO String
-- nasdaqStr = readFile nasdaqCSV

pih = "PIH,\"1347 Property Insurance Holdings, Inc.\""

-- https://hackage.haskell.org/package/cassava

getSymbols :: String -> Exchange -> IO [Stock]
getSymbols csvFilepath exchange = do
  csvData <- BL.readFile csvFilepath
  case decode NoHeader csvData of
    Left err -> do
      putStrLn err
      return []
    Right v -> do
      -- forM_ v $  \ (tk, dsp :: String) -> putStrLn $ tk ++ ": " ++ dsp
      V.toList <$> (forM v $ \ ( tk, dsp :: String) -> do
        ruuid <- randomIO :: IO UUID
        return $ Stock' ruuid tk dsp exchange)

-- use individual insert to avoid collisions on Ticker symbol
-- AlphaVantage doesn't distinguish between exchanges, anyway, so collisions are inevitable.
-- In fact, AlphaVantage data for any given symbol may be from an exchange I am not anticipating,
-- so the data may not belong to the stock I believe it to.
getSymbolsAndInsert :: String -> Exchange -> IO Int64
getSymbolsAndInsert csvFilepath exchange = do
  psqlConn <- getPsqlConnection commonFilePath
  stocks <- getSymbols csvFilepath exchange
  --count <- insertStocks stocks psqlConn
  count <- sum <$> mapM (\stock -> insertStock stock psqlConn) stocks
  closePsqlConnection psqlConn
  return count

insertNYSE = getSymbolsAndInsert "data/nyse.csv" nyse
insertNASDAQ = getSymbolsAndInsert "data/nasdaq.csv" nasdaq
insertAMEX = getSymbolsAndInsert "data/amex.csv" amex

