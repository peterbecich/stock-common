{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Types.Stock.JSON where

import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser, parse, parseMaybe)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Text.Internal (Text)
import qualified Data.Aeson as Aeson
import qualified Data.Map as Mp --  (Map, empty, keys)

import Types.Stock
import Types.Exchange.JSON

instance ToJSON Stock where
  toEncoding = genericToEncoding defaultOptions

