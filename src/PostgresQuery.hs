module PostgresQuery
  ( Postgres (..),
    createCategory,
    editCategory,
    editCategoryParent,
    executeBracket,
    queryAllCategory,
    createNews,
    queryUnpublishNews,
    queryNews,
    queryNewsLimitOffset,
    queryNewsOffset,
    queryNewsLimit,
    queryUnpublishNewsOffset,
    queryUnpublishNewsLimit,
    editNewsTitle,
    editNewsContent,
    editNewsPublish,
    createUser,
    queryUsers,
    queryUsersLimitOffset,
    queryUsersLimit,
    queryUsersOffset,
    editNewsCategory,
    queryImage,
    queryUser
  )
where

import Control.Exception (bracket)
import Control.Monad (unless, void)
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Database.PostgreSQL.Simple
  ( Binary (..),
    Connection,
    In (..),
    Only (..),
    Query (..),
    close,
    connectPostgreSQL,
    execute,
    executeMany,
    query,
    query_,
    returning,
  )
import Types (Category, ImG, News (..), NewsRow (..), User)

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
    defLimit :: Int
  }

executeBracket :: Postgres -> (Connection -> IO ()) -> IO ()
executeBracket Postgres {..} fun =
  bracket (connectPostgreSQL connString) close fun

createCategory :: Postgres -> Text -> Maybe Int -> Connection -> IO ()
createCategory Postgres {..} desc maybe_cat conn = do
  -- тут стоит понять, что я хочу избигать явного cat_id
  -- и пусть он мне его сам в таблицу ставит
  let insert = "INSERT INTO " <> tableCat <> " (cat_description, cat_parent) VALUES (?, ?)"
  void $ execute conn insert (desc, maybe_cat)

{-
editCategoryDescription :: Postgres -> Int -> Text -> Connection -> IO ()
editCategoryDescription Postgres {..} cat_id cat_description conn = do
  let update = "UPDATE " <> tableCat <> " SET cat_description = ? WHERE cat_id = ?"
  void $ execute conn update (cat_description, cat_id)
-}
editCategoryParent :: Postgres -> Int -> Maybe Int -> Connection -> IO ()
editCategoryParent Postgres {..} cat_id cat_parent conn = do
  let update = "UPDATE " <> tableCat <> " SET cat_parent = ? WHERE cat_id = ?"
  void $ execute conn update (cat_parent, cat_id)

editCategory :: Postgres -> Int -> Maybe Int -> Text -> Connection -> IO ()
editCategory Postgres {..} cat_id cat_parent cat_description conn = do
  let update =
        "UPDATE " <> tableCat
          <> " SET cat_parent = ? cat_description = ? WHERE cat_id = ?"
  void $ execute conn update (cat_parent, cat_description, cat_id)

queryAllCategory :: Postgres -> IO [Category]
queryAllCategory post@Postgres {..} = do
  let select = "SELECT * FROM " <> tableCat
  bracket (connectPostgreSQL connString) close (flip query_ select)

createNews ::
  Postgres ->
  Text ->
  Day ->
  Int ->
  Int ->
  Text ->
  Bool ->
  [Text] ->
  Connection ->
  IO ()
createNews
  Postgres {..}
  news_title
  news_create_date
  news_user_id
  news_cat_id
  news_content
  news_publish
  img_base64_list
  conn = do
    -- тут стоит понять, что я хочу избигать явного news_id
    -- и пусть он мне его сам в таблицу ставит
    let news_insert =
          "INSERT INTO " <> tableNewsRow
            <> " (news_title, news_create_date, news_user_id,"
            <> " news_cat_id, news_content, news_publish)"
            <> " VALUES (?, ?, ?, ?, ?, ?)"
            <> " RETURNING news_id"
    only_news_id_list <-
      query
        conn
        news_insert
        ( news_title,
          news_create_date,
          news_user_id,
          news_cat_id,
          news_content,
          news_publish
        )
    let news_id = head $ fmap fromOnly only_news_id_list :: Int
    --
    let imgs_insert =
          "INSERT INTO " <> tableImG
            <> " (img_base64) VALUES (?)"
            <> " RETURNING img_id"
    let only_img_base64_list = fmap Only img_base64_list
    only_img_id_list <- returning conn imgs_insert only_img_base64_list
    let img_id_list = fmap fromOnly only_img_id_list :: [Int]
    --
    let xs = [(news_id, x) | x <- img_id_list]
    let news_imgs_insert =
          "INSERT INTO " <> tableNewsImG
            <> " (news_id_many, img_id_many) VALUES (?, ?)"
    void $ executeMany conn news_imgs_insert xs

queryNews :: Postgres -> IO [News]
queryNews post@Postgres {..} =
  queryNewsLimitOffset post defLimit 0 True

queryNewsOffset :: Postgres -> Int -> IO [News]
queryNewsOffset post@Postgres {..} offset =
  queryNewsLimitOffset post defLimit offset True

queryNewsLimit :: Postgres -> Int -> IO [News]
queryNewsLimit post@Postgres {..} limit =
  queryNewsLimitOffset post limit 0 True

queryNewsLimitOffset :: Postgres -> Int -> Int -> Bool -> IO [News]
queryNewsLimitOffset Postgres {..} limit0 offset publish = do
  let limit = min limit0 defLimit
  print "hello2"
  let select =
        "SELECT * FROM "
          <> tableNewsRow
          <> " WHERE news_publish = ?"
          <> " LIMIT ? OFFSET ?"
  print $ select
  let myquery conn = query @_ @NewsRow conn select (publish, limit, offset)
  rows <- bracket (connectPostgreSQL connString) close myquery
  print "hello2"
  mapM action rows
  where
    action :: NewsRow -> IO News
    action row@NewsRow {news_id, news_user_id, news_cat_id} = do
      print "hello3"
      let userselect = "SELECT * FROM " <> tableUser <> " WHERE user_id = ?"
      let userquery conn = query @_ @User conn userselect (Only news_user_id)
      -- todo: logWarn if user not exist or we have many users
      users <- bracket (connectPostgreSQL connString) close userquery
      print "hello3"
      let user = head users
      --
      print "hello4"
      let catselect = "SELECT * FROM " <> tableCat <> " WHERE cat_id = ?"
      let catquery conn = query @_ @Category conn catselect (Only news_cat_id)
      -- todo: logWarn if user not exist or we have many cats
      cats <- bracket (connectPostgreSQL connString) close catquery
      print "hello4"
      let cat = head cats
      --
      print "hello5"
      let imgIdselect = "SELECT img_id_many FROM " <> tableNewsImG <> " WHERE news_id_many = ?"
      let imgIdquery conn = fmap (fmap fromOnly) $ query @_ @(Only Int) conn imgIdselect (Only news_id)
      imgids <- bracket (connectPostgreSQL connString) close imgIdquery
      --
      let imgselect = "SELECT * FROM " <> tableImG <> " WHERE img_id = ?"
      let imgsquery conn = query @_ @ImG conn imgselect (Only $ In imgids)
      imgs <- bracket (connectPostgreSQL connString) close imgsquery
      print "hello5"
      return
        News
          { news_row = row,
            news_user = user,
            news_cat = cat,
            news_imgs = imgs
          }

queryUnpublishNews :: Postgres -> IO [News]
queryUnpublishNews post@Postgres {..} =
  queryNewsLimitOffset post defLimit 0 False

queryUnpublishNewsOffset :: Postgres -> Int -> IO [News]
queryUnpublishNewsOffset post@Postgres {..} offset =
  queryNewsLimitOffset post defLimit offset False

queryUnpublishNewsLimit :: Postgres -> Int -> IO [News]
queryUnpublishNewsLimit post@Postgres {..} limit =
  queryNewsLimitOffset post limit 0 False

editNewsCategory :: Postgres -> Int -> Int -> Connection -> IO ()
editNewsCategory Postgres {..} news_id news_cat_id_new conn = do
  let update = "UPDATE " <> tableNewsRow <> " SET news_cat_id = ? WHERE news_id = ?"
  void $ execute conn update (news_cat_id_new, news_id)

editNewsTitle :: Postgres -> Int -> Text -> Connection -> IO ()
editNewsTitle Postgres {..} news_id news_title_new conn = do
  let update = "UPDATE " <> tableNewsRow <> " SET news_title = ? WHERE news_id = ?"
  void $ execute conn update (news_title_new, news_id)

editNewsContent :: Postgres -> Int -> Text -> Connection -> IO ()
editNewsContent Postgres {..} news_id news_content_new conn = do
  let update = "UPDATE " <> tableNewsRow <> " SET news_content = ? WHERE news_id = ?"
  void $ execute conn update (news_content_new, news_id)

editNewsPublish :: Postgres -> Int -> Bool -> Connection -> IO ()
editNewsPublish Postgres {..} news_id news_publish_new conn = do
  let update = "UPDATE " <> tableNewsRow <> " SET news_publish = ? WHERE news_id = ?"
  void $ execute conn update (news_publish_new, news_id)

createUser ::
  Postgres ->
  Text ->
  Text ->
  ByteString ->
  ByteString ->
  Day ->
  Bool ->
  Bool ->
  Connection ->
  IO ()
createUser
  Postgres {..}
  user_name
  user_login
  user_password
  user_salt
  user_create_date
  user_is_admin
  user_can_create_news
  conn = do
    -- тут стоит понять, что я хочу избигать явного user_id
    -- и пусть он мне его сам в таблицу ставит
    let insert =
          "INSERT INTO " <> tableUser
            <> " (user_name, user_login, user_password, user_salt,"
            <> " user_create_date, user_is_admin, user_can_create_news)"
            <> " VALUES (?, ?, ?, ?, ?, ?, ?)"
    void $
      execute
        conn
        insert
        ( user_name,
          user_login,
          (Binary user_password),
          (Binary user_salt),
          user_create_date,
          user_is_admin,
          user_can_create_news
        )

queryUsers :: Postgres -> IO [User]
queryUsers post@Postgres {..} =
  queryUsersLimitOffset post defLimit 0

queryUsersOffset :: Postgres -> Int -> IO [User]
queryUsersOffset post@Postgres {..} offset =
  queryUsersLimitOffset post defLimit offset

queryUsersLimit :: Postgres -> Int -> IO [User]
queryUsersLimit post@Postgres {..} limit =
  queryUsersLimitOffset post limit 0

queryUsersLimitOffset :: Postgres -> Int -> Int -> IO [User]
queryUsersLimitOffset Postgres {..} limit0 offset = do
  let limit = min limit0 defLimit
  let userselect =
        "SELECT * FROM "
          <> tableUser
          <> " LIMIT ? OFFSET ?"
  let userquery conn = query @_ @User conn userselect (limit, offset)
  bracket (connectPostgreSQL connString) close userquery

queryUser :: Postgres -> Text -> IO (Maybe User)
queryUser Postgres {..} login = do
  let select = "SELECT * FROM " <> tableUser <> " WHERE user_login = ?"
  let myquery conn = query conn select (Only login)
  ls <- bracket (connectPostgreSQL connString) close myquery
  case ls of 
    [] -> pure Nothing
    [a] -> pure $ Just a
    _ : _ -> pure Nothing

queryImage :: Postgres -> Int -> IO ImG
queryImage Postgres {..} img_id = do
  let select = "SELECT * FROM " <> tableImG <> " WHERE img_id = ?"
  let myquery conn = query conn select (Only img_id)
  head <$> bracket (connectPostgreSQL connString) close myquery
