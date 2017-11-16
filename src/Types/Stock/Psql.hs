
module Types.Stock.Psql where

import           Prelude

import           Data.Profunctor.Product (p4)
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
stocksTableStr :: String
stocksTableStr = "create table if not exists stocks"
       <> " ( stockId uuid not null primary key"
       <> " , symbol text not null"
       <> " , description text not null"
       <> " , exchange text not null"
       <> " );"

stocksTable :: Table
               ((Column P.PGUuid, Column P.PGText, Column P.PGText, Column P.PGText))
               ((Column P.PGUuid, Column P.PGText, Column P.PGText, Column P.PGText))
stocksTable = T.Table "stocks" (p4 ( required "stockId"
                          , required "symbol"
                          , required "description"
                          , required "exchange"
                          ))


stockToPsql :: Stock -> (Column P.PGUuid, Column P.PGText, Column P.PGText, Column P.PGText)
stockToPsql (Stock stockId symbol description (Exchange exchangeName _)) =
  (P.pgUUID stockId, P.pgString symbol, P.pgString description, P.pgString exchangeName)

insertStock :: Stock -> Connection -> IO Int64
insertStock stock connection =
  runInsert connection stocksTable (stockToPsql stock)

