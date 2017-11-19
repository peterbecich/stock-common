module Lib
    ( someFunc
    ) where

import DB.Psql
import DB.Redis
import Types.Tick
import Types.Tick.Psql
import Types.Stock
import Types.Stock.Psql
import Types.Stock.Redis
import Types.Exchange
import Types.Exchange.Psql

someFunc :: IO ()
someFunc = putStrLn "someFunc"
