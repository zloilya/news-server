module Config (Config (..), Tables (..)) where

import Data.Aeson (parseJSON)
import Data.String (fromString)
import Data.Text (Text)
import Data.Yaml (FromJSON)
import Database.PostgreSQL.Simple.Types (Query)
import GHC.Generics (Generic)

instance FromJSON Query where
  parseJSON = fmap fromString . parseJSON

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
    defSortNews :: Query,
    defSortUser :: Query,
    defSortCat :: Query,
    defSortImg :: Query,
    loglevel :: Priority,
    tables :: Tables
  }
  deriving (FromJSON, Generic, Eq, Show)

{-
tables in the database
-}
data Tables = Tables
  { tableUser :: Query,
    tableCat :: Query,
    tableImG :: Query,
    tableNewsImG :: Query,
    tableNewsRow :: Query
  }
  deriving (FromJSON, Generic, Eq, Ord, Show)
