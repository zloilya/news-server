module Lib (startSever) where

import Actions.Common (giveUser)
import Actions.Create (createCat, createNews, createUser)
import Actions.Edit (editCat, editNews)
import Actions.Get (getCat, getImage, getNews, getUnpublishNews, getUsers)
import Config (Config (..), Tables (..))
import Control.Concurrent (threadDelay)
import Crypto.KDF.PBKDF2 (fastPBKDF2_SHA256)
import Data.Aeson (ToJSON (toJSON), encode)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString.Lazy as LB
import Data.Text.Encoding (encodeUtf8)
import Encode (encodeNews, encodeUser, param)
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, Response, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Internal (Request (..))
import Network.Wai.Middleware.HttpAuth (AuthSettings (..), CheckCreds, basicAuth')
import PostgresQuery (Postgres (..), queryUser)
import Types
  ( Category (..),
    Choose (..),
    ImG (..),
    Limit' (..),
    News (..),
    NewsRow (..),
    User (..),
  )

authSettings :: AuthSettings
authSettings = "My Realm" {authIsProtected = canGuestLog}

canGuestLog :: Request -> IO Bool
canGuestLog req@Request {rawPathInfo} = do
  print req
  pure $ canGuest rawPathInfo

canGuest :: ByteString -> Bool
canGuest = \case
  "/news" -> False
  _ -> True

canUser :: ByteString -> Bool
canUser = \case
  "/unpublish_news" -> True
  "/edit_news" -> True
  _ -> False

check :: Postgres -> Request -> CheckCreds
check postgres Request {..} login password = do
  print $ (show login ++ " " ++ show password)
  user <- giveUser postgres requestHeaders
  pure $ case user of
    Left _ -> False
    Right User {..} ->
      fastPBKDF2_SHA256 param password user_salt == user_password

parseChoose :: Choose -> LB.ByteString
parseChoose c = case c of
  N ne -> foldMap encodeNews ne
  C ce -> encode $ toJSON ce
  U ue -> foldMap encodeUser ue
  I img -> encode $ toJSON img
  Nill -> "Nill"
  Error b -> fromStrict b
  Ok -> "Ok"

actionGET :: Postgres -> Request -> IO Choose
actionGET postgres req@Request {..} = do
  case rawPathInfo of
    "/news" -> getNews postgres req
    "/categorys" -> getCat postgres req
    "/unpublish_news" -> getUnpublishNews postgres req
    "/users" -> getUsers postgres req
    "/image" -> getImage postgres req
    _ -> pure $ Nill

actionPOST :: Postgres -> Request -> IO Choose
actionPOST postgres req@Request {..} = do
  case rawPathInfo of
    "/create_category" -> createCat postgres req
    "/create_news" -> createNews postgres req
    "/create_user" -> createUser postgres req
    _ -> pure $ Nill

actionPUT :: Postgres -> Request -> IO Choose
actionPUT postgres req@Request {..} = do
  case rawPathInfo of
    "/edit_category" -> editCat postgres req
    "/edit_news" -> editNews postgres req
    _ -> pure $ Nill

actionUpdate :: Postgres -> Request -> IO Choose
actionUpdate postgres req@Request {..} = do
  case requestMethod of
    "GET" -> actionGET postgres req
    "POST" -> actionPOST postgres req
    "PUT" -> actionPUT postgres req
    _ -> pure $ Nill

canAction :: Postgres -> Request -> IO Bool
canAction postgres req@Request {..} = do
  user <- giveUser postgres requestHeaders
  let userAdmin = case user of
        Left bs -> False
        Right User {..} -> user_is_admin
  pure $ canGuest rawPathInfo || canUser rawPathInfo || userAdmin

app :: Postgres -> Application
app postgres req respond = do
  -- todo: разделять на get и post
  print $ "new Request:"
  print $ "rawPathInfo =" ++ show (rawPathInfo req)
  print $ "queryString =" ++ show (queryString req)
  canWe <- canAction postgres req
  case canWe of
    False -> respond $ response404
    True -> do
      choose <- actionUpdate postgres req
      print choose
      respond $ response200 choose

response200 :: Choose -> Response
response200 choose =
  responseLBS
    status200
    [("Content-Type", "application/json; charset=utf-8")]
    $ parseChoose choose

response404 :: Response
response404 = responseLBS status404 [] LB.empty

{-
todo: верификация запросов
-}

startSever :: Config -> IO ()
startSever Config {..} = do
  let defLimit = Limit limit
      Tables {..} = tables
      connString = encodeUtf8 $ "host=" <> host <> " dbname=" <> dbname
      postgres = Postgres {..}
  print "start Sever"
  let logging = basicAuth' (check postgres) authSettings
  run 8080 (logging $ app postgres)
