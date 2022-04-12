-- {-# LANGUAGE ScopedTypeVariables #-}

module Lib
  ( startSever,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket_)
import Crypto.KDF.PBKDF2
  ( Parameters (..),
    fastPBKDF2_SHA256,
  )
import Crypto.Random (getRandomBytes)
import Data.Aeson (FromJSON (..), ToJSON (..), decode)
import Data.ByteString (ByteString, isPrefixOf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as LB
import Data.Either (fromLeft)
import Data.Foldable (find, foldl')
import Data.List (sortBy)
import Data.Text (Text, isInfixOf)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Debug.Trace (trace)
import Example (defNews1, defNews2, defNews3)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.URI (Query)
import Network.Wai
  ( Application,
    Middleware,
    Request (..),
    Response,
    ResponseReceived,
    responseLBS,
  )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Internal (Request (..))
import Network.Wai.Middleware.HttpAuth
  ( AuthSettings (authIsProtected),
    CheckCreds,
    basicAuth',
  )
import PostgresQuery
import Text.Read (readMaybe)
import Types
  ( Category (..),
    News (..),
    NewsRow (..),
    User (..), ImG
  )
import GHC.Generics (Generic)
import TextShow (showt)

data Choose
  = N [News]
  | C [Category]
  | U [User]
  | I ImG
  | Nill
  | Ok
  | Error ByteString

param :: Parameters
param = Parameters 4096 256

{-
dropBytes :: [b] -> ByteString -> ByteString
dropBytes word = B.drop (length word)
-}

filtredF ::
  (FromJSON a) =>
  (a -> News -> Bool) ->
  ByteString ->
  [News] ->
  [News]
filtredF pred j news =
  case decode (LB.fromStrict $ "\"" <> j <> "\"") of
    Nothing -> news
    Just a -> filter (pred a) news

createdF :: (Day -> Day -> Bool) -> ByteString -> [News] -> [News]
createdF f = filtredF pred
  where
    pred :: Day -> News -> Bool
    pred day News {news_row = NewsRow {news_create_date = day'}} = f day day'

authorF :: ByteString -> [News] -> [News]
authorF = undefined -- filtredF pred
{- where
  pred :: Text -> News -> Bool
  pred name News {news_user_name = name'} =
    name `isInfixOf` name' -}

catIdF :: ByteString -> [News] -> [News]
catIdF = filtredF pred
  where
    pred :: Int -> News -> Bool
    pred id News {news_row = NewsRow {news_cat_id = id'}} = id == id'

titleF :: ByteString -> [News] -> [News]
titleF = filtredF pred
  where
    pred :: Text -> News -> Bool
    pred text News {news_row = NewsRow {news_title = text'}} = text `isInfixOf` text'

contentF :: ByteString -> [News] -> [News]
contentF = filtredF pred
  where
    pred :: Text -> News -> Bool
    pred text News {news_row = NewsRow {news_content = text'}} = text `isInfixOf` text'

catTextF :: ByteString -> [News] -> [News]
catTextF = filtredF pred
  where
    pred :: Text -> News -> Bool
    pred dec News {news_row = NewsRow {news_cat_id = cat_id}} = do
      -- query
      let Category {cat_description = dec'} = undefined
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
sortByF "author" = sortBy undefined -- pred
{- where
  pred :: News -> News -> Ordering
  pred News {news_user_name = author} News {news_user_name = author'} =
    compare author author' -}
sortByF "category" = sortBy pred
  where
    pred :: News -> News -> Ordering
    pred
      News {news_row = NewsRow {news_cat_id = cat_id}}
      News {news_row = NewsRow {news_cat_id = cat_id'}} = do
        -- query
        let Category {cat_description = dec} = undefined
        let Category {cat_description = dec'} = undefined
        compare dec dec'
sortByF "photos" = sortBy pred
  where
    pred :: News -> News -> Ordering
    pred
      News {news_row = NewsRow {news_id = news_id}}
      News {news_row = NewsRow {news_id = news_id'}} =
        undefined
--compare (length author) (length author')
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

isField :: ByteString -> (ByteString, Maybe ByteString) -> Bool
isField x (y, _) = x == y

findInQuery :: ByteString -> Query -> Either ByteString ByteString
findInQuery name querys = case find (isField name) querys of
  Nothing -> Left $ "not find " <> name
  Just (_, Nothing) -> Left $ name <> " is empty"
  Just (_, Just bytes) -> Right bytes

myDecode :: (FromJSON a) => ByteString -> Maybe a
myDecode j = decode (LB.fromStrict $ "\"" <> j <> "\"")

myDude :: FromJSON b => Either ByteString ByteString -> Either ByteString b
myDude (Left bs) = Left bs
myDude (Right bs) = case (myDecode bs) of 
  Nothing -> Left $ "not a need type"
  Just a -> Right a

data ImG' = ImG' [Text]
  deriving (FromJSON, Generic)

toImG :: ImG' -> [ByteString] 
toImG (ImG' texts) = fmap encodeUtf8 texts

choose :: Postgres -> Request -> IO Choose
choose postgres Request {rawPathInfo = info, queryString = querys} = do
  a <- defNews1
  b <- defNews2
  c <- defNews3
  let news = [a, b, c]
  let exec = executeBracket postgres
  case info of
    "/news" -> do
       -- доступно всем
      let e_limit = myDude $ findInQuery "limit" querys
      let e_offset = myDude $ findInQuery "offset" querys
      N <$> case (e_limit, e_offset) of
        (Right limit, Right offset) ->
          queryNewsLimitOffset postgres limit offset True
        (Right limit, Left _) ->
          queryNewsLimit postgres limit
        (Left _, Right offset) ->
          queryNewsOffset postgres offset
        (Left _, Left _) ->
          queryNews postgres
      -- pure $ N $ foldl' actionFilter news querys
    "/categorys" ->
      -- доступно всем
      C <$> queryAllCategory postgres 
    "/create_category" -> do
      -- доступно админам
      let e_description = findInQuery "cat_description" querys
      let e_parent = case find (isField "cat_parent") querys of
            Nothing -> Left $ "not find " <> "cat_parent"
            Just (_, a) -> Right $ a
      case (e_description, e_parent) of
        (Right a, Right b) -> do
          exec $ createCategory postgres (decodeUtf8 a) (myDecode =<< b)
          return Ok
        (a, b) -> do
          let a' = fromLeft "" a
          let b' = fromLeft "" a
          return $ Error $ a' <> " " <> b'
    "/edit_category" -> do
      -- доступно админам
      let e_id = myDude $ findInQuery "cat_id" querys
      let e_description = findInQuery "cat_description" querys
      let e_parent = case find (isField "cat_parent") querys of
            Nothing -> Left $ "not find " <> "cat_parent"
            Just (_, a) -> Right $ a
      case (e_description, e_parent, e_id) of
        (Right a, Right b, Right c) -> do
          exec $ editCategory postgres c (myDecode =<< b) (decodeUtf8 a) 
          return Ok
        (Left _, Right b, Right c) -> do
          exec $ editCategoryParent postgres c (myDecode =<< b) 
          return Ok
        (a, b, c) -> do
          let a' = fromLeft "" a
          let b' = fromLeft "" a
          return $ Error $ a' <> " " <> b'
    "/create_news" -> do
      -- доступно особым юзерам
      let e_title = myDude $ findInQuery "news_title" querys
      day <- utctDay <$> getCurrentTime
      let e_user_id = myDude $ findInQuery "news_user_id" querys
      let e_cat_id = myDude $ findInQuery "news_cat_id" querys
      let e_content = myDude $ findInQuery "news_content" querys
      let e_publish = myDude $ findInQuery "news_publish" querys
      let e_imgs = fmap toImG $ myDude $ findInQuery "news_imgs" querys
      --
      -- скажем что клиент нам уже отдает содержимое в Base64
      -- let x = undefined :: ByteString
      -- let y = B64.encode x
      case (e_title, e_user_id, e_cat_id, e_content, e_publish, e_imgs) of
        (Right t, Right u, Right cat, Right c, Right p, Right i) -> do
          exec $ createNews postgres t day u cat c p i
          return Ok
        e_err ->
          return $ Error $ encodeUtf8 $ showt e_err
    "/unpublish_news" -> do
      -- доступно особым юзерам
      let e_limit = myDude $ findInQuery "limit" querys
      let e_offset = myDude $ findInQuery "offset" querys
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
      let e_news_id = myDude $ findInQuery "news_id"      querys
      let e_cat_id  = myDude $ findInQuery "news_cat_id"  querys
      let e_title   = myDude $ findInQuery "news_title"   querys
      let e_content = myDude $ findInQuery "news_content" querys
      let e_publish = myDude $ findInQuery "news_publish" querys
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
    "create_user" -> do
      -- доступно админам
      let e_name = myDude $ findInQuery "user_name" querys
      let e_login = myDude $ findInQuery "user_login" querys
      let e_password = findInQuery "user_password" querys
      day <- utctDay <$> getCurrentTime
      let e_is_admin = myDude $ findInQuery "user_is_admin" querys
      let e_can_create_news = myDude $ findInQuery "user_can_create_news" querys
      case (e_name, e_login, e_password, e_is_admin, e_can_create_news) of
        (Right name, Right login, Right password, Right is_admin, Right can) -> do
          salt <- getRandomBytes 16
          let hash = fastPBKDF2_SHA256 param password salt
          exec $ createUser postgres name login hash salt day is_admin can
          return Ok
        e_err ->
          return $ Error $ encodeUtf8 $ showt e_err
    "users" -> do
      -- доступно всем
      let e_limit = myDude $ findInQuery "limit" querys
      let e_offset = myDude $ findInQuery "offset" querys
      U <$> case (e_limit, e_offset) of
        (Right limit, Right offset) ->
          queryUsersLimitOffset postgres limit offset
        (Right limit, Left _) ->
          queryUsersLimit postgres limit
        (Left _, Right offset) ->
          queryUsersOffset postgres offset
        (Left _, Left _) ->
          queryUsers postgres
    "image" -> do
      -- доступно всем
      let e_img_id = myDude $ findInQuery "img_id" querys
      case e_img_id of 
        Left bs -> return $ Error bs
        Right img_id -> I <$> queryImage postgres img_id
    _ -> pure $ Nill -- can see only admin user i guess?

logging :: Middleware
logging = basicAuth' check authSettings

{-
todo:
поменять ответ authOnNoAuth с 401 на 404
-}
authSettings :: AuthSettings
authSettings = "My Realm" {authIsProtected = protected} :: AuthSettings

protected :: Request -> IO Bool
protected req = trace (show req) $
  pure $ case rawPathInfo req of
    "/news" -> False
    -- добавь доступное только админам
    _ -> True

check :: Request -> CheckCreds
check req login password =
  trace (show login ++ show password) $
    -- админы могут все
    -- узнать есть ли у юзера возможность создавать новости
    --     * неопубликованные - только их автор.
    --     * редактирование только автору новости.
    --     * получение только опубликованных картинок, иначе только автору
    pure True

app :: Postgres -> Application
app postgres req respond = do
  print $ "new Request:"
  print $ "rawPathInfo =" ++ show (rawPathInfo req)
  print $ "queryString =" ++ show (queryString req)
  c <- choose postgres req
  let bytes = case c of
        N ne -> undefined -- encode $ toJSON ne
        C ce -> undefined
        U ue -> undefined
        I img -> undefined
        Nill -> "Nill"
        Error b -> undefined
        Ok -> undefined
  respond $
    responseLBS
      status200
      [("Content-Type", "application/json; charset=utf-8")]
      bytes

{-
todo: верификация запросов
-}

startSever :: IO ()
startSever = do
  let host = undefined
  let dbname = undefined
  let postgres =
        Postgres
          { tableUser = "tableUser",
            tableCat = "tableCat",
            tableImG = "tableImG",
            tableNewsImG = "tableNewsImG",
            tableNewsRow = "tableNewsRow",
            connString = encodeUtf8 $ "host=" <> host <> " dbname=" <> dbname,
            defLimit = 20
          }
  print "start Sever"
  run 8080 (logging $ app postgres)
  threadDelay (20 * 1000000)
  putStrLn "Test suite not yet implemented"
