module Config where

import Data.Text (Text)
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple
  ( Connection,
    In (..),
    Only (..),
    Query (..))

{-
log levels
-}
data Priority
  = -- | Debug messages
    Debug
  | -- | Notable information that requires no immediate action.
    Info
  | -- | Something is probably wrong, and we should investigate.
    Warn
  deriving (FromJSON, Generic, Eq, Ord, Show)

{-
config
-}
data Config = Config
  { host :: Text,
    dbname :: Text,
    limit :: Int,
    loglevel :: Priority,
    tables :: Tables
  }
  deriving (FromJSON, Generic, Eq, Show)

{-
tables in the database
-}
data Tables = Tables
  { tableUserString :: String,
    tableCatString :: String,
    tableImGString :: String,
    tableNewsImGString :: String,
    tableNewsRowString :: String
  }
  deriving (FromJSON, Generic, Eq, Ord, Show)
