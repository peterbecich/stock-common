{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Types.Stock.JSON where

import Data.Aeson
import qualified Data.Aeson as Aeson

import Data.Aeson.Types (Parser, parse, parseMaybe)

import qualified Data.Map as Mp --  (Map, empty, keys)
import Data.Text.Internal (Text)
import Data.ByteString.Lazy.Char8 (pack, unpack)

import Control.Monad

import Types.Stock
import Types.Exchange.JSON

instance ToJSON Stock where
  toEncoding = genericToEncoding defaultOptions

