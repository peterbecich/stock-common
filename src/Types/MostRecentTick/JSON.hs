{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Types.MostRecentTick.JSON where

import Data.Aeson

import Types.MostRecentTick

instance ToJSON MostRecentTick where
  toEncoding = genericToEncoding defaultOptions


