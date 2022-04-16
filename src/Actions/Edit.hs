module Actions.Edit where

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

editNews :: Postgres -> Request -> IO Choose
editNews postgres Request {..} = do
  let exec = executeBracket postgres
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
          exec $ editNewsCategory postgres news_id cat_id
      case e_title of
        Left _ -> pure ()
        Right title ->
          exec $ editNewsTitle postgres news_id title
      case e_content of
        Left _ -> pure ()
        Right content ->
          exec $ editNewsContent postgres news_id content
      case e_publish of
        Left _ -> pure ()
        Right publish ->
          exec $ editNewsPublish postgres news_id publish
      return Ok

editCat :: Postgres -> Request -> IO Choose
editCat postgres Request {..} = do
  let exec = executeBracket postgres
  -- доступно админам
  let e_id = findAndDecode "cat_id" queryString
  let e_description = findInQuery "cat_description" queryString
  let parent = case find (isField "cat_parent") queryString of
        Nothing -> Nothing
        Just (_, a) -> a
  case (e_description, e_id) of
    (Right a, Right c) -> do
      exec $ editCategory postgres c (myDecode =<< parent) (decodeUtf8 a)
      return Ok
    (Left _, Right c) -> do
      exec $ editCategoryParent postgres c (myDecode =<< parent)
      return Ok
    e_err ->
      return $ Error $ encodeUtf8 $ showt e_err