module ActionUpdate ( actionUpdate ) where

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
import Network.HTTP.Types.URI (Query)
import Network.Wai.Internal (Request (..))
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

isField :: ByteString -> (ByteString, Maybe ByteString) -> Bool
isField x (y, _) = x == y

findInQuery :: ByteString -> Query -> Either ByteString ByteString
findInQuery name querys = case find (isField name) querys of
  Nothing -> Left $ "not find " <> name
  Just (_, Nothing) -> Left $ name <> " is empty"
  Just (_, Just bytes) -> Right bytes

myDecode :: (FromJSON a) => ByteString -> Maybe a
myDecode j = decode (LB.fromStrict $ "\"" <> j <> "\"")

myDecodeE :: FromJSON b => Either ByteString ByteString -> Either ByteString b
myDecodeE (Left bs) = Left bs
myDecodeE (Right bs) = case (myDecode bs) of
  Nothing -> case decode (LB.fromStrict bs) of
    Nothing -> Left $ "not a need type: " <> bs 
    Just a -> Right a
  Just a -> Right a

findAndDecode :: FromJSON b => ByteString -> Query -> Either ByteString b
findAndDecode bytes = myDecodeE . findInQuery bytes

toImG :: ImG -> Text
toImG (ImG _ texts) = texts

actionUpdate :: Postgres -> Request -> IO Choose
actionUpdate postgres Request {rawPathInfo = info, queryString = querys} = do
  let exec = executeBracket postgres
  case info of
    "/news" -> do
      -- доступно всем
      let e_limit = findAndDecode "limit" querys
      let e_offset = findAndDecode "offset" querys
      let filter news = foldl' actionFilter news querys
      -- todo: /news?sort_by=category
      -- если head category == sort_by, то идем в базу и делаем норм запрос
      news <- case (e_limit, e_offset) of
        (Right limit, Right offset) ->
          queryNewsLimitOffset postgres limit offset True
        (Right limit, Left _) ->
          queryNewsLimit postgres limit
        (Left _, Right offset) ->
          queryNewsOffset postgres offset
        (Left _, Left _) ->
          queryNews postgres
      return $ N $ filter news
    "/categorys" ->
      -- доступно всем
      C <$> queryAllCategory postgres
    "/create_category" -> do
      -- доступно админам
      let e_description = findInQuery "cat_description" querys
      let parent = case find (isField "cat_parent") querys of
            Nothing -> Nothing
            Just (_, a) -> a
      case (e_description) of
        (Right a) -> do
          exec $ createCategory postgres (decodeUtf8 a) (myDecode =<< parent)
          return Ok
        (Left a) -> 
          return $ Error $ a
    "/edit_category" -> do
      -- доступно админам
      let e_id = findAndDecode "cat_id" querys
      let e_description = findInQuery "cat_description" querys
      let parent = case find (isField "cat_parent") querys of
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
    "/create_news" -> do
      -- доступно особым юзерам
      let e_title = findAndDecode "news_title" querys
      day <- utctDay <$> getCurrentTime
      let e_user_id = findAndDecode "news_user_id" querys
      let e_cat_id = findAndDecode "news_cat_id" querys
      let e_content = findAndDecode "news_content" querys
      let e_publish = findAndDecode "news_publish" querys
      let e_imgs = fmap (fmap toImG) $ findAndDecode "news_imgs" querys
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
    "/unpublish_news" -> do
      -- доступно особым юзерам
      let e_limit = findAndDecode "limit" querys
      let e_offset = findAndDecode "offset" querys
      N <$> case (e_limit, e_offset) of
        (Right limit, Right offset) -> do
          queryNewsLimitOffset postgres limit offset False
        (Right limit, Left _) -> do
          queryUnpublishNewsLimit postgres limit
        (Left _, Right offset) -> do
          queryUnpublishNewsOffset postgres offset
        (Left _, Left _) -> do
          queryUnpublishNews postgres
    "/edit_news" -> do
      -- доступно особым юзерам
      let e_news_id = findAndDecode "news_id" querys
      let e_cat_id = findAndDecode "news_cat_id" querys
      let e_title = findAndDecode "news_title" querys
      let e_content = findAndDecode "news_content" querys
      let e_publish = findAndDecode "news_publish" querys
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
    "/create_user" -> do
      -- доступно админам
      let e_name = findAndDecode "user_name" querys
      let e_login = findAndDecode "user_login" querys
      let e_password = findInQuery "user_password" querys
      day <- utctDay <$> getCurrentTime
      let e_is_admin = findAndDecode "user_is_admin" querys
      let e_can_create_news = findAndDecode "user_can_create_news" querys
      case (e_name, e_login, e_password, e_is_admin, e_can_create_news) of
        (Right name, Right login, Right password, Right is_admin, Right can) -> do
          salt <- getRandomBytes 16
          let hash = fastPBKDF2_SHA256 param password salt
          exec $ createUser postgres name login hash salt day is_admin can
          return Ok
        e_err ->
          return $ Error $ encodeUtf8 $ showt e_err
    "/users" -> do
      -- доступно всем
      let e_limit = findAndDecode "limit" querys
      let e_offset = findAndDecode "offset" querys
      U <$> case (e_limit, e_offset) of
        (Right limit, Right offset) ->
          queryUsersLimitOffset postgres limit offset
        (Right limit, Left _) ->
          queryUsersLimit postgres limit
        (Left _, Right offset) ->
          queryUsersOffset postgres offset
        (Left _, Left _) ->
          queryUsers postgres
    "/image" -> do
      -- доступно всем
      let e_img_id = findAndDecode "img_id" querys
      case e_img_id of
        Left bs -> return $ Error bs
        Right img_id -> I <$> queryImage postgres img_id
    _ -> pure $ Nill -- can see only admin user i guess?
