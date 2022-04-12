module Example where

import Data.Time (Day)
import Data.Time.Clock (getCurrentTime, utctDay)
import Types



defDay :: IO Day
defDay = utctDay <$> getCurrentTime

admin :: User
admin =
  User
    { user_id = 1,
      user_name = "Admin",
      user_login = "admin",
      user_password = "123321",
      user_salt = "123456",
      user_create_date = toEnum 0,
      user_is_admin = True,
      user_can_create_news = True
    }

user :: User
user =
  admin
    { user_id = 2,
      user_name = "User",
      user_login = "user",
      user_is_admin = False
    }

defNews1 :: IO News
defNews1 = do
  day <- defDay
  let row = NewsRow
          { news_id = 1,
        news_title = "Def #1 news",
        news_create_date = day,
        news_user_id = user_id admin,
        --news_user_name = user_name admin,
        news_cat_id = 1, -- Category 1 "ccc" Nothing,
        news_content = "bbbb",
        news_publish = True
      }
  return $ News {
    news_row = undefined, 
    news_user = undefined, 
    news_cat = undefined,
    news_imgs = undefined
  }
  

defNews2 :: IO News
defNews2 = do
  day <- defDay
  let row = NewsRow
            { news_id = 2,
        news_title = "Def #2 news",
        news_create_date = day,
        news_user_id = user_id admin,
        --news_user_name = user_name admin,
        news_cat_id = 2, -- Category 2 "aaa" Nothing,
        news_content = "bbbb",
        news_publish = True
      }
  return $ News {
    news_row = undefined, 
    news_user = undefined, 
    news_cat = undefined,
    news_imgs = undefined
  }

defNews3 :: IO News
defNews3 = do
  day <- defDay
  let row = NewsRow
           { news_id = 3,
        news_title = "Def #3 news",
        news_create_date = day,
        news_user_id = user_id user,
        --news_user_name = user_name user,
        news_cat_id = 3, -- Category 3 "bbb" Nothing,
        news_content = "bbbb",
        news_publish = True
      }
  return $ News {
    news_row = undefined, 
    news_user = undefined, 
    news_cat = undefined,
    news_imgs = undefined
  }






