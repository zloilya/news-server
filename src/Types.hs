module Types
  ( ImG (..),
    Category (..),
    News (..),
    NewsRow (..),
    Choose (..),
    User (..),
    Content,
    Description,
    CanCreateNews,
    CatId,
    ImgId,
    IsAdmin,
    Limit,
    Login,
    Name,
    NewsId,
    Offset,
    Publish,
    Title,
    UserId,
    Offset' (..),
    UserId' (..),
    Limit' (..),
    Title' (..),
    CanCreateNews' (..),
    CatId' (..),
    Content' (..),
    Description' (..),
    ImgId' (..),
    IsAdmin' (..),
    Login' (..),
    Name' (..),
    NewsId' (..),
    Publish' (..),
  )
where

import Data.Aeson.Types (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (Day)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.Generics (Generic)
import TextShow (TextShow)

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
  deriving (Show, Generic, Eq, Ord, ToRow, FromRow)

data Category = Category
  { cat_id :: Int,
    cat_description :: Text,
    cat_parent :: Maybe Int
  }
  deriving (Show, ToJSON, Generic, Eq, Ord, ToRow, FromRow)

data ImG = ImG
  { img_id :: Int,
    img_base64 :: Text
  }
  deriving (Show, ToJSON, FromJSON, Generic, Eq, Ord, ToRow, FromRow)

data NewsImG = NewsImG
  { news_id_many :: Int,
    img_id_many :: Int
  }
  deriving (Show, Generic, Eq, Ord, ToRow, FromRow)

data NewsRow = NewsRow
  { news_id :: Int,
    news_title :: Text,
    news_create_date :: Day,
    news_user_id :: Int,
    news_cat_id :: Int,
    news_content :: Text,
    news_publish :: Bool
  }
  deriving (Show, ToJSON, Generic, Eq, Ord, ToRow, FromRow)

data News = News
  { news_row :: NewsRow,
    news_user :: User,
    news_cat :: Category,
    news_imgs :: [ImG]
  }
  deriving (Show, Eq, Ord)

data Choose
  = N [News]
  | C [Category]
  | U [User]
  | I ImG
  | Nill
  | Ok
  | Error ByteString
  deriving (Show)

newtype CatId' a = CatId a
  deriving newtype (Show, Eq, Ord, ToRow, Generic, ToField, FromJSON, TextShow)

type CatId = CatId' Int

newtype UserId' a = UserId a
  deriving newtype (Show, Eq, Ord, ToRow, Generic, ToField, FromJSON, TextShow)

type UserId = UserId' Int

newtype Offset' a = Offset a
  deriving newtype (Show, Eq, Ord, ToRow, Generic, ToField, FromJSON, TextShow)

type Offset = Offset' Int

newtype Limit' a = Limit a
  deriving newtype (Show, Eq, Ord, ToRow, Generic, ToField, FromJSON, TextShow)

type Limit = Limit' Int

newtype Description' a = Description a
  deriving newtype (Show, Eq, Ord, ToRow, Generic, ToField, FromJSON, TextShow)

type Description = Description' Text

newtype NewsId' a = NewsId a
  deriving newtype (Show, Eq, Ord, ToRow, Generic, ToField, FromJSON, TextShow)

type NewsId = NewsId' Int

newtype Title' a = Title a
  deriving newtype (Show, Eq, Ord, ToRow, Generic, ToField, FromJSON, TextShow)

type Title = Title' Text

newtype Content' a = Content a
  deriving newtype (Show, Eq, Ord, ToRow, Generic, ToField, FromJSON, TextShow)

type Content = Content' Text

newtype ImgId' a = ImgId a
  deriving newtype (Show, Eq, Ord, ToRow, Generic, ToField, FromJSON, TextShow)

type ImgId = ImgId' Int

newtype Name' a = Name a
  deriving newtype (Show, Eq, Ord, ToRow, Generic, ToField, FromJSON, TextShow)

type Name = Name' Text

newtype Login' a = Login a
  deriving newtype (Show, Eq, Ord, ToRow, Generic, ToField, FromJSON, TextShow)

type Login = Login' Text

newtype IsAdmin' a = IsAdmin a
  deriving newtype (Show, Eq, Ord, ToRow, Generic, ToField, FromJSON, TextShow)

type IsAdmin = IsAdmin' Bool

newtype CanCreateNews' a = CanCreateNews a
  deriving newtype (Show, Eq, Ord, ToRow, Generic, ToField, FromJSON, TextShow)

type CanCreateNews = CanCreateNews' Bool

newtype Publish' a = Publish a
  deriving newtype (Show, Eq, Ord, ToRow, Generic, ToField, FromJSON, TextShow)

type Publish = Publish' Bool
