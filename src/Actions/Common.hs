module Actions.Common where

import Data.Aeson (FromJSON, decode)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB
import Data.Foldable (find)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Tuple (swap)
import Database.Common (Postgres (..))
import qualified Database.Query as P (queryUser)
import Network.HTTP.Types (RequestHeaders, hAuthorization)
import Network.HTTP.Types.URI (Query)
import Network.Wai.Internal (Request (..))
import Network.Wai.Middleware.HttpAuth (extractBasicAuth)
import TextShow (showt)
import Types (ImG (ImG), User)

isField :: ByteString -> (ByteString, Maybe ByteString) -> Bool
isField x (y, _) = x == y

findInQuery :: ByteString -> Query -> Either ByteString ByteString
findInQuery name querys = case find (isField name) querys of
  Nothing -> Left $ "not find " <> name
  Just (_, Nothing) -> Left $ name <> " is empty"
  Just (_, Just bytes) -> Right bytes

findInQueryList :: ByteString -> Query -> ([ByteString], Query)
findInQueryList name = swap . fmap (mapMaybe snd) . break (isField name)

myDecode :: (FromJSON a) => ByteString -> Maybe a
myDecode j = decode (LB.fromStrict $ "\"" <> j <> "\"")

myDecodeE :: FromJSON b => ByteString -> Either ByteString b
myDecodeE bs = case (myDecode bs) of
  Nothing -> case decode (LB.fromStrict bs) of
    Nothing -> Left $ "not a need type: " <> bs
    Just a -> Right a
  Just a -> Right a

findAndDecode :: FromJSON b => ByteString -> Query -> Either ByteString b
findAndDecode bytes q = (myDecodeE =<<) $ findInQuery bytes q

toImG :: ImG -> Text
toImG (ImG _ texts) = texts

giveUser :: Postgres -> RequestHeaders -> IO (Either ByteString User)
giveUser postgres requestHeaders =
  case (lookup hAuthorization $ requestHeaders) >>= extractBasicAuth of
    Nothing -> do
      print "warning!!"
      pure $ Left $ "something go wrong"
    Just (login', _) -> do
      case (myDecodeE login') of
        Left bs -> pure $ Left bs
        Right login -> do
          userM <- P.queryUser postgres login
          case userM of
            Nothing -> do
              print "warning!!"
              pure $ Left $ "something go wrong"
            Just user -> pure $ Right user
