{-# LANGUAGE DeriveGeneric #-}

module Types.Tick where

import Data.Time.Clock (UTCTime)
import Data.UUID
import GHC.Generics

import Types.Stock (Stock)

data Tick' a b c d e f g =
  Tick' { time :: a
        , open :: b
        , high :: c
        , low :: d
        , close :: e
        , volume :: f
        , stock :: g
        } deriving (Generic, Show)

type Tick = Tick' UTCTime Double Double Double Double Int Stock

