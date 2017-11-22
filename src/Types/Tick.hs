{-# LANGUAGE DeriveGeneric #-}

module Types.Tick where

import Data.Time.Clock (UTCTime)

import GHC.Generics

import Types.Stock (Stock)

-- data Tick = Tick { time :: UTCTime
--                  , open :: Double
--                  , high :: Double
--                  , low :: Double
--                  , close :: Double
--                  , volume :: Int
--                  , stock :: Stock
--                  } deriving (Generic, Show)

type Tick' = Tick UTCTime Double Double Double Double Int

data Tick a b c d e f =
  Tick { time :: a
        , open :: b
        , high :: c
        , low :: d
        , close :: e
        , volume :: f
        , stock :: Stock
        } deriving (Generic, Show)

