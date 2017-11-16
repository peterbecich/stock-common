
module Types.Stock.Psql where

import           Prelude

import           Data.Profunctor.Product (p2)
import           Data.Profunctor.Product.Default (def)
import           Opaleye (Column, Table(Table),
                          required, optional, (.==), (.<),
                          PGInt4, PGFloat8)

import Control.Monad
import Data.Char
import Data.Functor ((<$>), fmap)
import Data.Int (Int64)
import Data.Map (Map, empty, size, mapKeys, toList, assocs)
import Data.Monoid
import Data.Time.LocalTime
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Internal (Connection)
import Opaleye.Manipulation
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Opaleye.Constant as C
import qualified Opaleye.Internal.Unpackspec as U
import qualified Opaleye.PGTypes as P
import qualified Opaleye.Table as T

import Types.Stock
import Types.Exchange

-- TODO foreign key
stocksTable :: String
stocksTable = "create table if not exists stocks"
       <> " ( stockId uuid not null primary key"
       <> " , symbol text not null"
       <> " , description text not null"
       <> " , exchange text not null"
       <> " );"


stockToPsql :: Stock -> (Column P.PGUuid, Column P.PGText, Column P.PGText, Column P.PGText)
stockToPsql (Stock stockId symbol description (Exchange exchangeName _)) =
  (P.pgUUID stockId, P.pgString symbol, P.pgString description, P.pgString exchangeName)

insertStock :: Stock -> Connection -> IO Int64
insertStock stock connection = undefined

