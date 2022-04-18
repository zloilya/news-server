module Actions.Edit where

import Actions.Common (findAndDecode, isField, myDecode)
import Data.ByteString (ByteString)
import Data.Foldable (find)
import Data.Text.Encoding (encodeUtf8)
import Database.Common (Postgres (..))
import qualified Database.Common as P (executeBracket)
import qualified Database.Edit as P
  ( editCategory,
    editCategoryParent,
    editNewsCategory,
    editNewsContent,
    editNewsPublish,
    editNewsTitle,
  )
import Network.Wai.Internal (Request (..))
import TextShow (showt)
import Types (Choose (Error, Ok))

editNews :: Postgres -> Request -> IO Choose
editNews postgres Request {..} = do
  let exec = P.executeBracket postgres
  -- доступно особым юзерам
  let e_news_id = findAndDecode "news_id" queryString
  let e_cat_id = findAndDecode "news_cat_id" queryString
  let e_title = findAndDecode "news_title" queryString
  let e_content = findAndDecode "news_content" queryString
  let e_publish = findAndDecode "news_publish" queryString
  case e_news_id of
    Left bs -> return $ Error bs
    Right news_id -> do
      case e_cat_id of
        Left _ -> pure ()
        Right cat_id ->
          exec $ P.editNewsCategory postgres news_id cat_id
      case e_title of
        Left _ -> pure ()
        Right title ->
          exec $ P.editNewsTitle postgres news_id title
      case e_content of
        Left _ -> pure ()
        Right content ->
          exec $ P.editNewsContent postgres news_id content
      case e_publish of
        Left _ -> pure ()
        Right publish ->
          exec $ P.editNewsPublish postgres news_id publish
      return Ok

editCat :: Postgres -> Request -> IO Choose
editCat postgres Request {..} = do
  let exec = P.executeBracket postgres
  -- доступно админам
  let e_id = findAndDecode "cat_id" queryString
  let e_description = findAndDecode "cat_description" queryString
  let parent = case find (isField "cat_parent") queryString of
        Nothing -> Nothing
        Just (_, a) -> a
  case (e_description, e_id) of
    (Right desc, Right cat_id) -> do
      exec $ P.editCategory postgres cat_id (myDecode =<< parent) desc
      return Ok
    (Left _, Right cat_id) -> do
      exec $ P.editCategoryParent postgres cat_id (myDecode =<< parent)
      return Ok
    e_err ->
      return $ Error $ encodeUtf8 $ showt e_err
