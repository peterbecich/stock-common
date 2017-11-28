{-# LANGUAGE DeriveGeneric #-}

module Types.Stock where

import Data.UUID (UUID)
import GHC.Generics

import Types.Exchange

data Stock' a b c d = Stock' { stockId :: a
                             , symbol :: b
                             , description :: c
                             , exchange :: d
                             } deriving (Generic, Show)

type Stock = Stock' UUID String String Exchange

instance (Eq a, Eq b, Eq c, Eq d) => Eq (Stock' a b c d)

applyExchange :: Stock' UUID String String (String, String, Int)
              -> Stock
applyExchange (Stock' stockId symbol description (exchangeName, exchangeTimeZone, exchangeTimeZoneOffset)) =
  Stock' stockId symbol description (Exchange' exchangeName exchangeTimeZone exchangeTimeZoneOffset)
  
