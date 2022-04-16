module Actions.Common where

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

giveUser :: Postgres -> RequestHeaders -> IO (Either ByteString User)
giveUser postgres requestHeaders =
  case (lookup hAuthorization $ requestHeaders) >>= extractBasicAuth of
    Nothing -> do
      print "warning!!"
      pure $ Left $ "something go wrong"
    Just (login, _) -> do
      userM <- queryUser postgres (decodeUtf8 login)
      case userM of
        Nothing -> do
          print "warning!!"
          pure $ Left $ "something go wrong"
        Just user -> pure $ Right user
