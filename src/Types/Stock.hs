{-# LANGUAGE DeriveGeneric #-}

module Types.Stock where

import GHC.Generics

import Data.UUID (UUID)

import Types.Exchange

data Stock' a b c d = Stock' { stockId :: a
                             , symbol :: b
                             , description :: c
                             , exchange :: d
                             } deriving (Generic, Show)

type Stock = Stock' UUID String String Exchange

