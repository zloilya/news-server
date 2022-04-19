module Actions.Get where

import Actions.Common (findAndDecode, findInQueryList, giveUser)
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.String (IsString (..))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
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
import Network.HTTP.Types (Query)
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

todo: сделать везде ORDER BY, иначе хз странно это как-то
todo: переделать фильтр, так как они имеют аналогичную багу
-}

getNews :: Postgres -> Request -> IO Choose
getNews postgres Request {..} = do
  -- доступно всем
  let e_limit = findAndDecode "limit" queryString
  let e_offset = findAndDecode "offset" queryString

  let (ordersB, filters) = findInQueryList "sort_by" queryString
  let filter news = foldl' actionFilter news filters
  -- todo: трансфер day и прочие в поля news типа news_create_data
  -- или оставить это верификации? может это и есть верификация?
  let e_orders = mapM (fmap (fromString . T.unpack)) (fmap decodeUtf8' ordersB)

  e_news <- case (e_limit, e_offset, e_orders) of
    (Right limit, Right offset, Right orders) ->
      Right <$> P.queryNewsLimitOffset postgres limit offset orders
    (Right limit, Left _, Right orders) ->
      Right <$> P.queryNewsLimit postgres limit orders
    (Left _, Right offset, Right orders) ->
      Right <$> P.queryNewsOffset postgres offset orders
    (Left _, Left _, Right orders) ->
      Right <$> P.queryNews postgres orders
    e_err ->
      return $ Left $ encodeUtf8 $ showt e_err
  pure $ case e_news of
    Left bs -> Error bs
    Right news -> case filter (Right news) of
      (Left bs) -> Error bs
      (Right news) -> N news

getCat :: Postgres -> Request -> IO Choose
getCat postgres Request {..} = do
  -- todo: filter cats
  let filter cats = foldl' actionFilter cats queryString

  -- доступно всем
  C <$> P.queryAllCategory postgres

getUnpublishNews :: Postgres -> Request -> IO Choose
getUnpublishNews postgres Request {..} = do
  -- доступно юзерам
  e_user_id <- (pure . fmap (UserId . user_id)) =<< giveUser postgres requestHeaders
  let e_limit = findAndDecode "limit" queryString
  let e_offset = findAndDecode "offset" queryString

  let (ordersB, filters) = findInQueryList "sort_by" queryString
  let filter news = foldl' actionFilter news filters
  let e_orders = mapM (fmap (fromString . T.unpack)) (fmap decodeUtf8' ordersB)

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

  let (ordersB, filters) = findInQueryList "sort_by" queryString
  let filter news = foldl' actionFilter news filters
  let e_orders = mapM (fmap (fromString . T.unpack)) (fmap decodeUtf8' ordersB)

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
