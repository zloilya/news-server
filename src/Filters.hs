module Filters (actionFilter) where

import Data.Aeson (FromJSON, decode)
import Data.ByteString (ByteString, isPrefixOf)
import Data.ByteString.Lazy (fromStrict)
import Data.List (sortBy)
import Data.Text (Text, isInfixOf)
import Data.Time (Day)
import Types (Category (..), Choose (..), News (..), NewsRow (..), User (..))

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

actionFilter :: Either ByteString [News] -> (ByteString, Maybe ByteString) -> Either ByteString [News]
actionFilter (Right news) pair = actionFilter' news pair
actionFilter (Left e) _ = Left e

actionFilter' :: [News] -> (ByteString, Maybe ByteString) -> Either ByteString [News]
actionFilter' news (_, Nothing) = Right news
actionFilter' news (key, Just value) = case key of
  "created_at" -> Right $ createdF (==) value news
  "created_until" -> Right $ createdF (>) value news
  "created_since" -> Right $ createdF (<) value news
  "author" -> Right $ authorF value news
  "category_id" -> Right $ catIdF value news
  "category_description" -> Right $ catTextF value news
  "title" -> Right $ titleF value news
  "content" -> Right $ contentF value news
  "sort_by" -> Right $ sortByF value news
  _ -> Left $ "unidentified filter"
