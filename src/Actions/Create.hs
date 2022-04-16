module Actions.Create where

import Actions.Common
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
  )
import TextShow (showt)
import Types
  ( Category (..),
    Choose (..),
    ImG (..),
    News (..),
    NewsRow (..),
    User (..),
    param,
  )

createCat :: Postgres -> Request -> IO Choose
createCat postgres Request {..} = do
  let exec = executeBracket postgres
  -- доступно админам
  let e_description = findInQuery "cat_description" queryString
  let parent = case find (isField "cat_parent") queryString of
        Nothing -> Nothing
        Just (_, a) -> a
  case (e_description) of
    (Right a) -> do
      exec $ createCategory postgres (decodeUtf8 a) (myDecode =<< parent)
      return Ok
    (Left a) ->
      return $ Error $ a

createNews' :: Postgres -> Request -> IO Choose
createNews' postgres Request {..} = do
  let exec = executeBracket postgres
  -- доступно особым юзерам
  let e_title = findAndDecode "news_title" queryString
  day <- utctDay <$> getCurrentTime
  let e_user_id = findAndDecode "news_user_id" queryString
  let e_cat_id = findAndDecode "news_cat_id" queryString
  let e_content = findAndDecode "news_content" queryString
  let e_publish = findAndDecode "news_publish" queryString
  let e_imgs = fmap (fmap toImG) $ findAndDecode "news_imgs" queryString
  --
  -- скажем что клиент нам уже отдает содержимое в Base64
  -- let img = bytes :: ByteString
  -- let img64 = B64.encode img
  case (e_title, e_user_id, e_cat_id, e_content, e_publish, e_imgs) of
    (Right t, Right u, Right cat, Right c, Right p, Right i) -> do
      exec $ createNews postgres t day u cat c p i
      return Ok
    e_err ->
      return $ Error $ encodeUtf8 $ showt e_err

createUser' :: Postgres -> Request -> IO Choose
createUser' postgres Request {..} = do
  let exec = executeBracket postgres
  -- доступно админам
  let e_name = findAndDecode "user_name" queryString
  let e_login = findAndDecode "user_login" queryString
  let e_password = findInQuery "user_password" queryString
  day <- utctDay <$> getCurrentTime
  let e_is_admin = findAndDecode "user_is_admin" queryString
  let e_can_create_news = findAndDecode "user_can_create_news" queryString
  case (e_name, e_login, e_password, e_is_admin, e_can_create_news) of
    (Right name, Right login, Right password, Right is_admin, Right can) -> do
      salt <- getRandomBytes 16
      let hash = fastPBKDF2_SHA256 param password salt
      exec $ createUser postgres name login hash salt day is_admin can
      return Ok
    e_err ->
      return $ Error $ encodeUtf8 $ showt e_err
