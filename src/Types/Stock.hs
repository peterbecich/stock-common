{-# LANGUAGE DeriveGeneric #-}

module Types.Stock where

import GHC.Generics

import Data.UUID (UUID)

import Types.Exchange (Exchange)


data Stock = Stock { stockId :: UUID
                   , symbol :: String
                   , description :: String
                   , exchange :: Exchange
                   } deriving (Generic, Show)
