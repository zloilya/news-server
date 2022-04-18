module Actions.Get where

import Actions.Common (findAndDecode, giveUser)
import Data.Foldable (foldl')
import Data.Text.Encoding (encodeUtf8)
import Database.Common (Postgres (..))
import qualified Database.Query as P
  ( queryAllCategory,
    queryImage,
    queryNews,
    queryNewsLimit,
    queryNewsLimitOffset,
    queryNewsOffset,
    queryUnpublishNews,
    queryUnpublishNewsLimit,
    queryUnpublishNewsLimitOffset,
    queryUnpublishNewsOffset,
    queryUsers,
    queryUsersLimit,
    queryUsersLimitOffset,
    queryUsersOffset,
  )
import Filters (actionFilter)
import Network.Wai.Internal (Request (..))
import TextShow (showt)
import Types (Choose (..), User (..), UserId' (..))

{-
эм sortByF работает, только при всех новостях,
а вот когда новости не все, то сортирует только свежие,
что конечно не сильно ожидаемое поведение
SELECT
    column_list
FROM
    table1
ORDER BY
    column_list
LIMIT row_count OFFSET offset;
-}

getNews :: Postgres -> Request -> IO Choose
getNews postgres Request {..} = do
  -- доступно всем
  let e_limit = findAndDecode "limit" queryString
  let e_offset = findAndDecode "offset" queryString
  let filter news = foldl' actionFilter news queryString
  -- todo: /news?sort_by=category
  -- если head category == sort_by, то идем в базу и делаем норм запрос
  news <- case (e_limit, e_offset) of
    (Right limit, Right offset) ->
      P.queryNewsLimitOffset postgres limit offset
    (Right limit, Left _) ->
      P.queryNewsLimit postgres limit
    (Left _, Right offset) ->
      P.queryNewsOffset postgres offset
    (Left _, Left _) ->
      P.queryNews postgres
  pure $ case filter (Right news) of
    (Left bs) -> Error bs
    (Right news) -> N news

getCat :: Postgres -> Request -> IO Choose
getCat postgres _ = do
  -- доступно всем
  C <$> P.queryAllCategory postgres

getUnpublishNews :: Postgres -> Request -> IO Choose
getUnpublishNews postgres Request {..} = do
  -- доступно юзерам
  e_user_id <- (pure . fmap (UserId . user_id)) =<< giveUser postgres requestHeaders
  let e_limit = findAndDecode "limit" queryString
  let e_offset = findAndDecode "offset" queryString
  case (e_user_id, e_limit, e_offset) of
    (Right user_id, Right limit, Right offset) ->
      N <$> P.queryUnpublishNewsLimitOffset postgres limit offset user_id
    (Right user_id, Right limit, Left _) ->
      N <$> P.queryUnpublishNewsLimit postgres limit user_id
    (Right user_id, Left _, Right offset) ->
      N <$> P.queryUnpublishNewsOffset postgres offset user_id
    (Right user_id, Left _, Left _) ->
      N <$> P.queryUnpublishNews postgres user_id
    e_err ->
      pure $ Error $ encodeUtf8 $ showt e_err

getUsers :: Postgres -> Request -> IO Choose
getUsers postgres Request {..} = do
  -- доступно всем
  let e_limit = findAndDecode "limit" queryString
  let e_offset = findAndDecode "offset" queryString
  U <$> case (e_limit, e_offset) of
    (Right limit, Right offset) ->
      P.queryUsersLimitOffset postgres limit offset
    (Right limit, Left _) ->
      P.queryUsersLimit postgres limit
    (Left _, Right offset) ->
      P.queryUsersOffset postgres offset
    (Left _, Left _) ->
      P.queryUsers postgres

getImage :: Postgres -> Request -> IO Choose
getImage postgres Request {..} = do
  -- доступно всем
  let e_img_id = findAndDecode "img_id" queryString
  case e_img_id of
    Left bs -> return $ Error bs
    Right img_id -> I <$> P.queryImage postgres img_id
