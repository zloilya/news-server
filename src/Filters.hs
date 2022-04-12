module Filters where

import Data.Aeson (FromJSON (..), ToJSON (..), decode, encode)
import Data.ByteString (ByteString, isPrefixOf)
import Data.ByteString.Lazy (fromStrict)
import Data.List (sortBy)
import Data.Text (Text, isInfixOf)
import Data.Time (Day)
import Types (Category (..), News (..), NewsRow (..), User (..))

filtredF ::
  (FromJSON a) =>
  (a -> News -> Bool) ->
  ByteString ->
  [News] ->
  [News]
filtredF pred j news =
  case decode (fromStrict $ "\"" <> j <> "\"") of
    Nothing -> news
    Just a -> filter (pred a) news

createdF :: (Day -> Day -> Bool) -> ByteString -> [News] -> [News]
createdF comp = filtredF pred
  where
    pred :: Day -> News -> Bool
    pred day News {news_row = NewsRow {news_create_date = day'}} =
      comp day day'

authorF :: ByteString -> [News] -> [News]
authorF = filtredF pred
  where
    pred :: Text -> News -> Bool
    pred name News {news_user = User {user_name = name'}} =
      -- let name' = view (news_user . user_name) news
      name `isInfixOf` name'

catIdF :: ByteString -> [News] -> [News]
catIdF = filtredF pred
  where
    pred :: Int -> News -> Bool
    pred id News {news_row = NewsRow {news_cat_id = id'}} = id == id'

titleF :: ByteString -> [News] -> [News]
titleF = filtredF pred
  where
    pred :: Text -> News -> Bool
    pred text News {news_row = NewsRow {news_title = text'}} =
      text `isInfixOf` text'

contentF :: ByteString -> [News] -> [News]
contentF = filtredF pred
  where
    pred :: Text -> News -> Bool
    pred text News {news_row = NewsRow {news_content = text'}} =
      text `isInfixOf` text'

catTextF :: ByteString -> [News] -> [News]
catTextF = filtredF pred
  where
    pred :: Text -> News -> Bool
    pred dec News {news_cat = Category {cat_description = dec'}} = do
      -- let dec' = view (news_cat . cat_description) news
      dec `isInfixOf` dec'

{-
эм sortByF работает, только при всех новостях,
а вот когда новости не все, то сортирует только свежие,
что конечно не сильно ожидаемое поведение
SELECT
    column_list
FROM
    table1
ORDER BY column_list
LIMIT row_count OFFSET offset;
-}
sortByF :: ByteString -> [News] -> [News]
sortByF "day" = sortBy pred
  where
    pred :: News -> News -> Ordering
    pred
      News {news_row = NewsRow {news_create_date = day}}
      News {news_row = NewsRow {news_create_date = day'}} =
        compare day day'
sortByF "author" = sortBy pred
  where
    pred :: News -> News -> Ordering
    pred
      News {news_user = User {user_name = author}}
      News {news_user = User {user_name = author'}} =
        compare author author'
sortByF "category" = sortBy pred
  where
    pred :: News -> News -> Ordering
    pred
      News {news_cat = Category {cat_description = dec}}
      News {news_cat = Category {cat_description = dec'}} =
        compare dec dec'
sortByF "photos" = sortBy pred
  where
    pred :: News -> News -> Ordering
    pred News {news_imgs = imgs} News {news_imgs = imgs'} =
      compare (length imgs) (length imgs')
sortByF _ = id

actionFilter :: [News] -> (ByteString, Maybe ByteString) -> [News]
actionFilter news (_, Nothing) = news
actionFilter news (key, Just value) = case key of
  "created_at" -> createdF (==) value news
  "created_until" -> createdF (>) value news
  "created_since" -> createdF (<) value news
  "author" -> authorF value news
  "category_id" -> catIdF value news
  "category_description" -> catTextF value news
  "title" -> titleF value news
  "content" -> contentF value news
  "sort_by" -> sortByF value news
  _ -> news -- 404 ???
