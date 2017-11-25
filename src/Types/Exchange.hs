{-# LANGUAGE DeriveGeneric #-}

module Types.Exchange where

import Data.Time.LocalTime (TimeZone)
import GHC.Generics
import Opaleye (Column)
import Opaleye.PGTypes

data Exchange' a b c = Exchange' { name :: a
                             , timeZone :: b
                             , timeZoneOffset :: c
                             } deriving (Generic, Show)

type Exchange = Exchange' String String Int

