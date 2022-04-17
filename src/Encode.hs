module Encode (encodeNews, encodeUser, param) where

import Crypto.KDF.PBKDF2 (Parameters (Parameters))
import Data.Aeson (KeyValue ((.=)), encode, object)
import qualified Data.ByteString.Lazy as LB
import Types (News (..), User (..))

param :: Parameters
param = Parameters 4096 256

encodeNews :: News -> LB.ByteString
encodeNews News {..} =
  encode $
    object
      [ "news_row" .= news_row,
        "news_user_name" .= (user_name news_user),
        "news_cat" .= news_cat,
        "news_imgs" .= news_imgs
      ]

encodeUser :: User -> LB.ByteString
encodeUser User {..} =
  encode $
    object
      [ "user_id" .= user_id,
        "user_name" .= user_name,
        "user_create_date" .= user_create_date,
        "user_is_admin" .= user_is_admin,
        "user_can_create_news" .= user_can_create_news
      ]
