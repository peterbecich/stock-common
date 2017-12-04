module Lib
    ( someFunc
    ) where

import DB.Psql
import DB.Redis
import Types.Exchange
import Types.Exchange.JSON
import Types.Exchange.Psql
import Types.MostRecentTick
import Types.MostRecentTick.JSON
import Types.MostRecentTick.Redis
import Types.Stock
import Types.Stock.JSON
import Types.Stock.Psql
import Types.Tick
-- import Types.Tick.Cassandra
import Types.Tick.JSON
import Types.Tick.Psql
import Util.LoadStocks

someFunc :: IO ()
someFunc = putStrLn "someFunc"
 
