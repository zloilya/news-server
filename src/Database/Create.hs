module Database.Create where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (Day)
import Database.Common (Postgres (..))
import Database.PostgreSQL.Simple
  ( Binary,
    Connection,
    Only (Only, fromOnly),
    execute,
    executeMany,
    query,
    returning,
  )
import Types
  ( CanCreateNews,
    CatId,
    Content,
    Description,
    IsAdmin,
    Login,
    Name,
    Publish,
    Title,
    UserId,
  )

createCategory :: Postgres -> Description -> Maybe Int -> Connection -> IO ()
createCategory Postgres {..} cat_description cat_parent conn = do
  let insert = "INSERT INTO " <> tableCat <> " (cat_description, cat_parent) VALUES (?, ?)"
  void $ execute conn insert (cat_description, cat_parent)

createNews ::
  Postgres ->
  Title ->
  Day ->
  UserId ->
  CatId ->
  Content ->
  Publish ->
  [Text] ->
  Connection ->
  IO ()
createNews Postgres {..} title date user_id cat_id content publish imgs conn = do
  let insert =
        "INSERT INTO " <> tableNewsRow
          <> " (news_title, news_create_date, news_user_id,"
          <> " news_cat_id, news_content, news_publish, news_imgs_len)"
          <> " VALUES (?, ?, ?, ?, ?, ?, ?)"
          <> " RETURNING news_id"
  let len = length imgs
  only_news_id_list <- query conn insert (title, date, user_id, cat_id, content, publish, len)
  let news_id = head $ fmap fromOnly only_news_id_list :: Int
  --
  let imgs_insert =
        "INSERT INTO " <> tableImG
          <> " (img_base64) VALUES (?)"
          <> " RETURNING img_id"
  let only_img_base64_list = fmap Only imgs
  only_img_id_list <- returning conn imgs_insert only_img_base64_list
  let img_id_list = fmap fromOnly only_img_id_list :: [Int]
  --
  let xs = [(news_id, x) | x <- img_id_list]
  let news_imgs_insert =
        "INSERT INTO " <> tableNewsImG
          <> " (news_id_many, img_id_many) VALUES (?, ?)"
  void $ executeMany conn news_imgs_insert xs

createUser ::
  Postgres ->
  Name ->
  Login ->
  Binary ByteString ->
  Binary ByteString ->
  Day ->
  IsAdmin ->
  CanCreateNews ->
  Connection ->
  IO ()
createUser Postgres {..} name login password salt date is_admin can conn = do
  let insert =
        "INSERT INTO " <> tableUser
          <> " (user_name, user_login, user_password, user_salt,"
          <> " user_create_date, user_is_admin, user_can_create_news)"
          <> " VALUES (?, ?, ?, ?, ?, ?, ?)"
  void $ execute conn insert (name, login, password, salt, date, is_admin, can)
