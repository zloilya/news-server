
CREATE TABLE tableUser (
  user_id SERIAL PRIMARY KEY,
  user_name text NOT NULL, 
  user_login text NOT NULL UNIQUE,
  user_password bytea NOT NULL,
  user_salt bytea NOT NULL,
  user_create_date date NOT NULL DEFAULT CURRENT_DATE,
  user_is_admin boolean NOT NULL,
  user_can_create_news boolean NOT NULL
);

CREATE TABLE tableCat (
  cat_id SERIAL PRIMARY KEY,
  cat_description text NOT NULL, 
  cat_parent int
);

CREATE TABLE tableImG (
  img_id SERIAL PRIMARY KEY,
  img_base64 text NOT NULL
);

CREATE TABLE tableNewsImG (
  news_id_many int NOT NULL,
  img_id_many int NOT NULL
);

CREATE TABLE tableNewsRow (
  news_id SERIAL PRIMARY KEY,
  news_title text NOT NULL,
  news_create_date date NOT NULL,
  news_user_id int NOT NULL,
  news_cat_id int NOT NULL,
  news_content text NOT NULL,
  news_publish boolean NOT NULL
);