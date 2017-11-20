{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Types.Exchange.JSON where

import Data.Aeson
import qualified Data.Aeson as Aeson

import Data.Aeson.Types (Parser, parse, parseMaybe)
import qualified Data.Text as Txt
import qualified Data.Map as Mp --  (Map, empty, keys)
import Data.Text.Internal (Text)
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Data.Time.LocalTime

import Control.Monad

import Types.Exchange

instance ToJSON TimeZone where
  -- toJSON tz = object ["timeZONE" .= (show tz)]
  -- TODO this JSON object is nested too deep. fix
  toJSON tz = String (Txt.pack (show tz))

instance ToJSON Exchange where
  toEncoding = genericToEncoding defaultOptions
