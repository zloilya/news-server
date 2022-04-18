module Filters (actionFilter) where

import Data.Aeson (FromJSON, decode)
import Data.ByteString (ByteString, isPrefixOf)
import Data.ByteString.Lazy (fromStrict)
import Data.List (sortBy)
import Data.Text (Text, isInfixOf)
import Data.Time (Day)
import Types (Category (..), ImG, News (..), NewsRow (..), User (..))

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
    pred day news = comp day (news_create_date . news_row $ news)

authorF :: ByteString -> [News] -> [News]
authorF = filtredF pred
  where
    pred :: Text -> News -> Bool
    pred name news =
      name `isInfixOf` (user_name . news_user $ news)

catIdF :: ByteString -> [News] -> [News]
catIdF = filtredF pred
  where
    pred :: Int -> News -> Bool
    pred id news = id == (news_cat_id . news_row $ news)

titleF :: ByteString -> [News] -> [News]
titleF = filtredF pred
  where
    pred :: Text -> News -> Bool
    pred text news = text `isInfixOf` (news_title . news_row $ news)

contentF :: ByteString -> [News] -> [News]
contentF = filtredF pred
  where
    pred :: Text -> News -> Bool
    pred text news = text `isInfixOf` (news_content . news_row $ news)

catTextF :: ByteString -> [News] -> [News]
catTextF = filtredF pred
  where
    pred :: Text -> News -> Bool
    pred dec news = dec `isInfixOf` (cat_description . news_cat $ news)

compareF :: Ord a => (News -> a) -> News -> News -> Ordering
compareF get news news' = compare (get news) (get news')

sortByF :: ByteString -> [News] -> [News]
sortByF "day" = sortBy (compareF (news_create_date . news_row))
sortByF "author" = sortBy (compareF (user_name . news_user))
sortByF "category" = sortBy (compareF (cat_description . news_cat))
sortByF "photos" = sortBy (compareF (length . news_imgs))
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
