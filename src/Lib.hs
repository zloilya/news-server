module Lib (startSever) where

import ActionUpdate (actionUpdate)
import Control.Concurrent (threadDelay)
import Data.Aeson (FromJSON (..), KeyValue ((.=)), ToJSON (..), encode, object)
import Data.Aeson.Types (Value)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (Day)
import Debug.Trace (trace)
import Filters
import GHC.Generics (Generic)
import Network.HTTP.Types (status200)
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
import PostgresQuery (Postgres (..))
import Types
  ( Category (..),
    Choose (..),
    ImG (..),
    News (..),
    NewsRow (..),
    User (..),
  )
import Data.ByteString.Lazy (fromStrict)
import Config (Config (..), Tables(..))
import Data.String (IsString(..))
import qualified Data.Text as T

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

parseChoose :: Choose -> LB.ByteString
parseChoose c = case c of
  N ne -> foldMap fun ne
    where
      fun News {..} =
        encode $
          object
            [ "news_row" .= news_row,
              "news_cat" .= news_cat,
              "news_imgs" .= news_imgs
            ]
  C ce -> encode $ toJSON ce
  U ue -> foldMap fun ue
    where
      fun User {..} =
        encode $
          object
            [ "user_id" .= user_id,
              "user_name" .= user_name,
              "user_create_date" .= user_create_date,
              "user_is_admin" .= user_is_admin,
              "user_can_create_news" .= user_can_create_news
            ]
  I img -> encode $ toJSON img
  Nill -> "Nill"
  Error b -> fromStrict b
  Ok -> "Ok"

app :: Postgres -> Application
app postgres req respond = do
  -- todo: разделять на get и post
  print $ "new Request:"
  print $ "rawPathInfo =" ++ show (rawPathInfo req)
  print $ "queryString =" ++ show (queryString req)
  choose <- actionUpdate postgres req
  respond $
    responseLBS
      status200
      [("Content-Type", "application/json; charset=utf-8")]
      $ parseChoose choose

{-
todo: верификация запросов
-}

startSever :: Config -> IO ()
startSever Config{..} = do
  let defLimit = 20
  let Tables {..} = tables
  let tableUser = fromString tableUserString
  let tableCat = fromString tableCatString
  let tableImG = fromString tableImGString
  let tableNewsImG = fromString tableNewsImGString
  let tableNewsRow = fromString tableNewsRowString
  let connString = encodeUtf8 $ "host=" <> host <> " dbname=" <> dbname
  let postgres = Postgres { .. }
  print "start Sever"
  run 8080 (logging $ app postgres)
