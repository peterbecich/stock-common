{-# LANGUAGE DeriveGeneric #-}

module Types.Exchange where

import Data.Time.LocalTime (TimeZone)

import Opaleye (Column)
import Opaleye.PGTypes

import GHC.Generics

data Exchange' a b c = Exchange' { name :: a
                             , timeZone :: b
                             , timeZoneOffset :: c
                             } deriving (Generic, Show)

type Exchange = Exchange' String String Int

