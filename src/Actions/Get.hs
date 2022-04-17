module Actions.Get where

import Actions.Common (findAndDecode, giveUser)
import Crypto.KDF.PBKDF2
  ( Parameters (..),
    fastPBKDF2_SHA256,
  )
import Crypto.Random (getRandomBytes)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Aeson.Types (Value)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LB
import Data.Either (fromLeft)
import Data.Foldable (find, foldl')
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Filters (actionFilter)
import GHC.Generics (Generic)
import Network.HTTP.Types (RequestHeaders, hAuthorization)
import Network.HTTP.Types.URI (Query)
import Network.Wai.Internal (Request (..))
import Network.Wai.Middleware.HttpAuth (extractBasicAuth)
import PostgresQuery
  ( Postgres (..),
    createCategory,
    createNews,
    createUser,
    editCategory,
    editCategoryParent,
    editNewsCategory,
    editNewsContent,
    editNewsPublish,
    editNewsTitle,
    executeBracket,
    queryAllCategory,
    queryImage,
    queryNews,
    queryNewsLimit,
    queryNewsLimitOffset,
    queryNewsOffset,
    queryUnpublishNews,
    queryUnpublishNewsLimit,
    queryUnpublishNewsOffset,
    queryUser,
    queryUsers,
    queryUsersLimit,
    queryUsersLimitOffset,
    queryUsersOffset,
    queryUnpublishNewsLimitOffset,
  )
import TextShow (showt)
import Types
  ( Category (..),
    Choose (..),
    ImG (..),
    News (..),
    NewsRow (..),
    User (..),
    param, UserId' (UserId)
  )

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
      queryNewsLimitOffset postgres limit offset
    (Right limit, Left _) ->
      queryNewsLimit postgres limit
    (Left _, Right offset) ->
      queryNewsOffset postgres offset
    (Left _, Left _) ->
      queryNews postgres
  return $ N $ filter news

getCat :: Postgres -> Request -> IO Choose
getCat postgres _ = do
  -- доступно всем
  C <$> queryAllCategory postgres

getUnpublishNews :: Postgres -> Request -> IO Choose
getUnpublishNews postgres Request {..} = do
  -- доступно особым юзерам
  e_user_id <- (pure . fmap (UserId . user_id)) =<< giveUser postgres requestHeaders
  let e_limit = findAndDecode "limit" queryString
  let e_offset = findAndDecode "offset" queryString
  case (e_user_id, e_limit, e_offset) of
    (Right user_id, Right limit, Right offset) ->
      N <$> queryUnpublishNewsLimitOffset postgres limit offset user_id
    (Right user_id, Right limit, Left _) ->
      N <$> queryUnpublishNewsLimit postgres limit user_id
    (Right user_id, Left _, Right offset) ->
      N <$> queryUnpublishNewsOffset postgres offset user_id
    (Right user_id, Left _, Left _) ->
      N <$> queryUnpublishNews postgres user_id
    e_err -> 
      pure $ Error $ encodeUtf8 $ showt e_err

getUsers :: Postgres -> Request -> IO Choose
getUsers postgres Request {..} = do
  -- доступно всем
  let e_limit = findAndDecode "limit" queryString
  let e_offset = findAndDecode "offset" queryString
  U <$> case (e_limit, e_offset) of
    (Right limit, Right offset) ->
      queryUsersLimitOffset postgres limit offset
    (Right limit, Left _) ->
      queryUsersLimit postgres limit
    (Left _, Right offset) ->
      queryUsersOffset postgres offset
    (Left _, Left _) ->
      queryUsers postgres

getImage :: Postgres -> Request -> IO Choose
getImage postgres Request {..} = do
  -- доступно всем
  let e_img_id = findAndDecode "img_id" queryString
  case e_img_id of
    Left bs -> return $ Error bs
    Right img_id -> I <$> queryImage postgres img_id
