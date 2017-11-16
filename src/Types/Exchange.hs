{-# LANGUAGE DeriveGeneric #-}

module Types.Exchange where

import Data.Time.LocalTime (TimeZone)

import GHC.Generics

data Exchange = Exchange { name :: String
                         , timeZone :: TimeZone
                         } deriving (Generic, Show)
