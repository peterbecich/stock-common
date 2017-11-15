{-# LANGUAGE DeriveGeneric #-}

module Types.Tick where

import GHC.Generics

data Tick = Tick { open :: Double
                 , high :: Double
                 , low :: Double
                 , close :: Double
                 , volume :: Int
                 } deriving (Generic, Show)
