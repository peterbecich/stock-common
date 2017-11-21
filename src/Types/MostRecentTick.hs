{-# LANGUAGE DeriveGeneric #-}

module Types.MostRecentTick where

import GHC.Generics

import Data.UUID (UUID)

import Data.Time.Clock (UTCTime)

data MostRecentTick = MostRecentTick { stockId :: UUID
                                     , timestamp :: UTCTime
                                     } deriving (Generic, Show)


