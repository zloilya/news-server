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
    queryUser,
    queryUnpublishNewsLimitOffset,
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
import Types
  ( CanCreateNews,
    CatId,
    Category,
    Content,
    Description,
    ImG,
    ImgId,
    IsAdmin,
    Limit,
    Login,
    Name,
    News (..),
    NewsId,
    NewsRow (..),
    Offset,
    Offset' (Offset),
    Title,
    User,
    UserId,
  )

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
    defLimit :: Limit
  }

executeBracket :: Postgres -> (Connection -> IO ()) -> IO ()
executeBracket Postgres {..} fun =
  bracket (connectPostgreSQL connString) close fun

createCategory :: Postgres -> Description -> Maybe Int -> Connection -> IO ()
createCategory Postgres {..} cat_description cat_parent conn = do
  let insert = "INSERT INTO " <> tableCat <> " (cat_description, cat_parent) VALUES (?, ?)"
  void $ execute conn insert (cat_description, cat_parent)

editCategoryParent :: Postgres -> CatId -> Maybe Int -> Connection -> IO ()
editCategoryParent Postgres {..} cat_id cat_parent conn = do
  let update = "UPDATE " <> tableCat <> " SET cat_parent = ? WHERE cat_id = ?"
  void $ execute conn update (cat_parent, cat_id)

editCategory :: Postgres -> CatId -> Maybe Int -> Description -> Connection -> IO ()
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
  Title ->
  Day ->
  UserId ->
  CatId ->
  Content ->
  Bool ->
  [Text] ->
  Connection ->
  IO ()
createNews Postgres {..} title date user_id cat_id content publish imgs conn = do
  let insert =
        "INSERT INTO " <> tableNewsRow
          <> " (news_title, news_create_date, news_user_id,"
          <> " news_cat_id, news_content, news_publish)"
          <> " VALUES (?, ?, ?, ?, ?, ?)"
          <> " RETURNING news_id"
  only_news_id_list <- query conn insert (title, date, user_id, cat_id, content, publish)
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

rowToNews :: Postgres -> NewsRow -> IO News
rowToNews Postgres {..} row@NewsRow {news_id, news_user_id, news_cat_id} = do
  print "hello3"
  let userselect = "SELECT * FROM " <> tableUser <> " WHERE user_id = ?"
  let userquery conn = query conn userselect (Only news_user_id)
  -- todo: logWarn if user not exist or we have many users
  users <- bracket (connectPostgreSQL connString) close userquery
  print "hello3"
  let user = head users
  --
  print "hello4"
  let catselect = "SELECT * FROM " <> tableCat <> " WHERE cat_id = ?"
  let catquery conn = query conn catselect (Only news_cat_id)
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
  let imgsquery conn = query conn imgselect (Only $ In imgids)
  imgs <- bracket (connectPostgreSQL connString) close imgsquery
  print "hello5"
  return
    News
      { news_row = row,
        news_user = user,
        news_cat = cat,
        news_imgs = imgs
      }

queryNews :: Postgres -> IO [News]
queryNews post@Postgres {..} =
  queryNewsLimitOffset post defLimit (Offset 0)

queryNewsOffset :: Postgres -> Offset -> IO [News]
queryNewsOffset post@Postgres {..} offset =
  queryNewsLimitOffset post defLimit offset

queryNewsLimit :: Postgres -> Limit -> IO [News]
queryNewsLimit post@Postgres {..} limit =
  queryNewsLimitOffset post limit (Offset 0)

queryNewsLimitOffset :: Postgres -> Limit -> Offset -> IO [News]
queryNewsLimitOffset post@Postgres {..} limit0 offset = do
  let limit = min limit0 defLimit
  print "hello2"
  let select =
        "SELECT * FROM "
          <> tableNewsRow
          <> " WHERE news_publish = ?"
          <> " LIMIT ? OFFSET ?"
  print $ select
  let myquery conn = query conn select (True, limit, offset)
  rows <- bracket (connectPostgreSQL connString) close myquery
  print "hello2"
  mapM (rowToNews post) rows

queryUnpublishNews :: Postgres -> UserId -> IO [News]
queryUnpublishNews post@Postgres {..} =
  queryUnpublishNewsLimitOffset post defLimit (Offset 0)

queryUnpublishNewsOffset :: Postgres -> Offset -> UserId -> IO [News]
queryUnpublishNewsOffset post@Postgres {..} =
  queryUnpublishNewsLimitOffset post defLimit

queryUnpublishNewsLimit :: Postgres -> Limit -> UserId -> IO [News]
queryUnpublishNewsLimit post@Postgres {..} limit =
  queryUnpublishNewsLimitOffset post limit (Offset 0)

queryUnpublishNewsLimitOffset :: Postgres -> Limit -> Offset -> UserId -> IO [News]
queryUnpublishNewsLimitOffset post@Postgres {..} limit0 offset user_id = do
  let limit = min limit0 defLimit
  print "hello2"
  let select =
        "SELECT * FROM "
          <> tableNewsRow
          <> " WHERE news_publish = ? AND news_user_id = ?"
          <> " LIMIT ? OFFSET ?"
  print $ select
  let myquery conn = query conn select (False, user_id, limit, offset)
  rows <- bracket (connectPostgreSQL connString) close myquery
  print "hello2"
  mapM (rowToNews post) rows

editNewsCategory :: Postgres -> NewsId -> CatId -> Connection -> IO ()
editNewsCategory Postgres {..} news_id news_cat_id conn = do
  let update = "UPDATE " <> tableNewsRow <> " SET news_cat_id = ? WHERE news_id = ?"
  void $ execute conn update (news_cat_id, news_id)

editNewsTitle :: Postgres -> NewsId -> Title -> Connection -> IO ()
editNewsTitle Postgres {..} news_id news_title conn = do
  let update = "UPDATE " <> tableNewsRow <> " SET news_title = ? WHERE news_id = ?"
  void $ execute conn update (news_title, news_id)

editNewsContent :: Postgres -> NewsId -> Content -> Connection -> IO ()
editNewsContent Postgres {..} news_id news_content conn = do
  let update = "UPDATE " <> tableNewsRow <> " SET news_content = ? WHERE news_id = ?"
  void $ execute conn update (news_content, news_id)

editNewsPublish :: Postgres -> NewsId -> Bool -> Connection -> IO ()
editNewsPublish Postgres {..} news_id news_publish conn = do
  let update = "UPDATE " <> tableNewsRow <> " SET news_publish = ? WHERE news_id = ?"
  void $ execute conn update (news_publish, news_id)

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

queryUsers :: Postgres -> IO [User]
queryUsers post@Postgres {..} =
  queryUsersLimitOffset post defLimit (Offset 0)

queryUsersOffset :: Postgres -> Offset -> IO [User]
queryUsersOffset post@Postgres {..} offset =
  queryUsersLimitOffset post defLimit offset

queryUsersLimit :: Postgres -> Limit -> IO [User]
queryUsersLimit post@Postgres {..} limit =
  queryUsersLimitOffset post limit (Offset 0)

queryUsersLimitOffset :: Postgres -> Limit -> Offset -> IO [User]
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

queryImage :: Postgres -> ImgId -> IO ImG
queryImage Postgres {..} img_id = do
  let select = "SELECT * FROM " <> tableImG <> " WHERE img_id = ?"
  let myquery conn = query conn select (Only img_id)
  head <$> bracket (connectPostgreSQL connString) close myquery
