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
import Types.Exchange.Psql (nasdaq)
import Types.Stock.Psql (insertStocks)

import DB.Psql

nasdaqCSV = "data/nasdaq.csv"

-- nasdaqStr :: IO String
-- nasdaqStr = readFile nasdaqCSV

pih = "PIH,\"1347 Property Insurance Holdings, Inc.\""

-- https://hackage.haskell.org/package/cassava

getSymbols :: IO [Stock]
getSymbols = do
  csvData <- BL.readFile nasdaqCSV
  case decode NoHeader csvData of
    Left err -> do
      putStrLn err
      return []
    Right v -> do
      -- forM_ v $  \ (tk, dsp :: String) -> putStrLn $ tk ++ ": " ++ dsp
      V.toList <$> (forM v $ \ ( tk, dsp :: String) -> do
        ruuid <- randomIO :: IO UUID
        return $ Stock ruuid tk dsp nasdaq)

getSymbolsAndInsert :: IO Int64
getSymbolsAndInsert = do
  psqlConn <- getPsqlConnection commonFilePath
  stocks <- getSymbols
  count <- insertStocks stocks psqlConn
  closePsqlConnection psqlConn
  return count
