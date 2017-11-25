{-# LANGUAGE DeriveGeneric #-}

module Types.MostRecentTick where

import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics

data MostRecentTick = MostRecentTick { stockId :: UUID
                                     , timestamp :: UTCTime
                                     } deriving (Generic, Show)


