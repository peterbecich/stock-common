{-# LANGUAGE DeriveGeneric #-}

module Types.Tick where

import Data.Time.Clock (UTCTime)

import GHC.Generics

import Types.Stock (Stock)

data Tick = Tick { time :: UTCTime
                 , open :: Double
                 , high :: Double
                 , low :: Double
                 , close :: Double
                 , volume :: Int
                 , stock :: Stock
                 } deriving (Generic, Show)
