module Database.Edit where

import Control.Monad (void)
import Database.Common (Postgres (..))
import Database.PostgreSQL.Simple (Connection, execute)
import Types (CatId, Content, Description, NewsId, Publish, Title)

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

editNewsPublish :: Postgres -> NewsId -> Publish -> Connection -> IO ()
editNewsPublish Postgres {..} news_id news_publish conn = do
  let update = "UPDATE " <> tableNewsRow <> " SET news_publish = ? WHERE news_id = ?"
  void $ execute conn update (news_publish, news_id)
