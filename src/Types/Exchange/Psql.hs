
module Types.Exchange.Psql where

import           Prelude

import           Data.Profunctor.Product (p3)
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

import Types.Exchange

exchangeTableStr :: String
exchangeTableStr = "create table if not exists exchanges"
       <> " ( name text not null primary key"
       <> ", timeZone text not null"
       <> ", timeZoneOffset int4 not null"
       <> " );"

exchangeTable :: Table
                 ((Column P.PGText
                  , Column P.PGText
                  , Column P.PGInt4 ))
                 ((Column P.PGText
                  , Column P.PGText
                  , Column P.PGInt4 ))
exchangeTable = T.Table "exchanges" (p3 ( required "name"
                                        , required "timeZone"
                                        , required "timeZoneOffset" ))


exchangeToPsql :: Exchange -> (Column P.PGText, Column P.PGText, Column P.PGInt4)
exchangeToPsql (Exchange exchangeName timeZone) =
  let tzName = show timeZone
      tzOffset :: Int
      tzOffset = read $ timeZoneOffsetString timeZone
  in (P.pgString exchangeName, P.pgString tzName, P.pgInt4 tzOffset)


insertExchange :: Exchange -> Connection -> IO Int64
insertExchange exchange connection =
  runInsert connection exchangeTable (exchangeToPsql exchange)