module Database.Query where

import Control.Exception (bracket)
import Database.Common (Postgres (..))
import Database.PostgreSQL.Simple
  ( In (..),
    Only (..),
    close,
    connectPostgreSQL,
    query,
    query_,
  )
import Types
  ( Category,
    ImG,
    ImgId,
    Limit,
    Login,
    News (..),
    NewsRow (..),
    Offset,
    Offset' (..),
    User,
    UserId,
  )

queryAllCategory :: Postgres -> IO [Category]
queryAllCategory post@Postgres {..} = do
  let select = "SELECT * FROM " <> tableCat
  bracket (connectPostgreSQL connString) close (flip query_ select)

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

queryUser :: Postgres -> Login -> IO (Maybe User)
queryUser Postgres {..} login = do
  let select = "SELECT * FROM " <> tableUser <> " WHERE user_login = ?"
  let myquery conn = query conn select (Only login)
  ls <- bracket (connectPostgreSQL connString) close myquery
  case ls of
    [] -> pure Nothing
    [a] -> pure $ Just a
    _ : _ -> pure Nothing -- logWarn? login is unique => it impossible

queryImage :: Postgres -> ImgId -> IO ImG
queryImage Postgres {..} img_id = do
  let select = "SELECT * FROM " <> tableImG <> " WHERE img_id = ?"
  let myquery conn = query conn select (Only img_id)
  head <$> bracket (connectPostgreSQL connString) close myquery
