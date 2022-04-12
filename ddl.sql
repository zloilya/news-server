/* DROP DATABASE news_server; */ 
CREATE DATABASE news_server;

/* DROP TABLE news_table; */

/*
data User = User {
  user_id :: Int,
  user_name :: Text,
  user_login :: Text,
  user_password :: ByteString,
  user_salt :: ByteString,
  user_create_date :: Day,
  user_is_admin :: Bool,
  user_can_create_news :: Bool
} deriving (Generic, Eq, Ord, FromRow)
*/

CREATE TYPE user (
  name text NOT NULL,
  login text NOT NULL, 
  password text NOT NULL
);

/*
data Category = Category {
  cat_id :: Int,
  cat_description :: Text,
  cat_list :: Maybe Category
} deriving (ToJSON, Generic, Eq, Ord)
*/
/* 
data News = News {
  news_id :: Int,
  news_title :: Text,
  news_create_date :: Day,
  news_user_id :: Int,
  news_user_name :: Text,
  news_cat :: Category,
  news_content :: Text,
  news_path_photos :: [FilePath],
  news_publish :: Bool
} deriving (ToJSON, Generic, Eq, Ord)
*/

CREATE TABLE news_table (
  id integer PRIMARY KEY,
  title text NOT NULL,
  create_date date NOT NULL,
  count integer NOT NULL
);