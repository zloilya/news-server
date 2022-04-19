module Database.Common where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple (Connection, Query, close, connectPostgreSQL)
import Types (Limit)

type Table = Query

{-
Postgres enviroment
-}
data Postgres = Postgres
  { tableUser :: Table,
    tableCat :: Table,
    tableImG :: Table,
    tableNewsImG :: Table,
    tableNewsRow :: Table,
    connString :: ByteString,
    defLimit :: Limit,
    defSortNews :: Table,
    defSortUser :: Table,
    defSortCat :: Table,
    defSortImg :: Table
  }

executeBracket :: Postgres -> (Connection -> IO ()) -> IO ()
executeBracket Postgres {..} fun =
  bracket (connectPostgreSQL connString) close fun
