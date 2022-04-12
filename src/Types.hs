module Types
  ( User (..),
    Category (..),
    NewsRow (..),
    NewsImG (..),
    News(..),
    ImG(..),
    Choose(..),
    param
  )
where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (Day)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)
import Crypto.KDF.PBKDF2 (Parameters (Parameters))
import Data.Aeson.Types (FromJSON, ToJSON)

data User = User
  { user_id :: Int,
    user_name :: Text,
    user_login :: Text,
    user_password :: ByteString,
    user_salt :: ByteString,
    user_create_date :: Day,
    user_is_admin :: Bool,
    user_can_create_news :: Bool
  }
  deriving (Generic, Eq, Ord, ToRow, FromRow)

data Category = Category
  { cat_id :: Int,
    cat_description :: Text,
    cat_parent :: Maybe Int
  }
  deriving (ToJSON, Generic, Eq, Ord, ToRow, FromRow)

data ImG = ImG
  { img_id :: Int,
    img_base64 :: Text
  }
  deriving (ToJSON, FromJSON, Generic, Eq, Ord, ToRow, FromRow)

data NewsImG = NewsImG
  { news_id_many :: Int,
    img_id_many :: Int
  }
  deriving (Generic, Eq, Ord, ToRow, FromRow)

data NewsRow = NewsRow
  { news_id :: Int,
    news_title :: Text,
    news_create_date :: Day,
    news_user_id :: Int,
    news_cat_id :: Int,
    news_content :: Text,
    news_publish :: Bool
  }
  deriving (ToJSON, Generic, Eq, Ord, ToRow, FromRow)

data News = News
  { news_row :: NewsRow,
    news_user :: User,
    news_cat :: Category,
    news_imgs :: [ImG]
  }
  deriving (Eq, Ord)

data Choose
  = N [News]
  | C [Category]
  | U [User]
  | I ImG
  | Nill
  | Ok
  | Error ByteString

param :: Parameters
param = Parameters 4096 256
