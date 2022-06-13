{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--  Module      : Database.hs
--  Description : Used for performing queries on the sqlite database
--
--  This module is used for performing queries on the sqlite database.
--  It has the following functionality:
--
--  1. Inserting and updating of data in users and tweets in the sqlite database based on the results returned from a twitter api request
--
--  2. Downloading data from the sqlite database to various JSON files
--
--   3. Initialising the database and creating users and tweets tables
module Database (initDB, saveUser, saveTweet, saveTweets, querySavedUsers, queryUsers, queryTweets, queryTweetsUsers, querySavedTweetsByUser) where

import Control.Applicative
-- Needed for encoding json
import Data.Aeson
import Data.Aeson.Text as DAT
import qualified Data.Char
import Data.Dynamic
import qualified Data.Text as DT
import qualified Data.Text.Lazy.IO as I
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.ToRow
import Fetch
import GHC.Generics (Generic)
import Parse
import Types

-- ##########################################################################################################################################

data UserField = UserField String String String (Maybe Bool) (Maybe String) String (Maybe String) Int Int Int deriving (Show)

instance FromRow UserField where
  fromRow = UserField <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

data TweetField = TweetField String String String String Int Int Int Int deriving (Show)

instance FromRow TweetField where
  fromRow = TweetField <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- ##########################################################################################################################################

-- Users
instance ToJSON Userdb where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Userdb

instance FromRow Userdb where
  fromRow = Userdb <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- Tweets
instance ToJSON Tweetdb where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Tweetdb

instance FromRow Tweetdb where
  fromRow = Tweetdb <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- Tweets and Users

instance ToJSON TweetUserdb where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON TweetUserdb

instance FromRow TweetUserdb where
  fromRow = TweetUserdb <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

-- ##########################################################################################################################################

-- |
--  Creates my twietwejf and tweets table in sqlite database (haskell-project/database.sqlite)
initDB :: IO Connection
initDB = do
  conn <- open "database.sqlite"
  --runRaw conn "PRAGMA foreign_keys = ON"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS users (\
    \user_id VARCHAR(20) PRIMARY KEY,\
    \username VARCHAR(15) NOT NULL, \
    \name VARCHAR(15) NOT NULL, \
    \verified BOOLEAN NOT NULL, \
    \location VARCHAR(50)  NULL, \
    \created_at VARCHAR(15) NOT NULL, \
    \bio VARCHAR(160)  NULL, \
    \following_count INT DEFAULT NULL, \
    \tweet_count INT DEFAULT NULL, \
    \followers_count INT DEFAULT NULL)"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS tweets (\
    \tweet_id VARCHAR(20) PRIMARY KEY,\
    \fk_user_id VARCHAR(20),\
    \tweeted_at VARCHAR(40) NULL, \
    \contents VARCHAR(250)  NULL, \
    \quote_count INT DEFAULT NULL, \
    \retweet_count INT DEFAULT NULL, \
    \like_count INT DEFAULT NULL, \
    \reply_count INT DEFAULT NULL, \
    \search_query VARCHAR(250)  NULL, \
    \FOREIGN KEY('fk_user_id') REFERENCES 'users'('user_id'))"
  return conn

-- ##########################################################################################################################################

-- |
--  Checks whether the given user exists in the users table using their user_id.
--  Inserts a user if they do not exist and updates the existing entry if they do.
saveUser ::
  -- | Connection to sqlite database
  Connection ->
  -- | User object
  User ->
  -- | For printing success/failure messages to the user
  IO ()
saveUser conn my_user = do
  --print (dynTypeRep (toDyn my_user))
  let my_user_id = user_id my_user
  let my_user_metric = user_metrics my_user
  -- Check if User already exists
  checkUser <- queryNamed conn "SELECT * FROM users WHERE user_id = :user_id" [":user_id" := my_user_id] :: IO [UserField]
  --print $ length checkUser
  if null checkUser
    then do
      -- Add User to Database
      print "Adding user to database"
      execute conn "INSERT INTO users (user_id, username, name, verified, location, created_at, bio, following_count, tweet_count, followers_count) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" (user_id my_user, map Data.Char.toLower $ username my_user, name my_user, verified my_user, location my_user, created_at my_user, bio my_user, following_count my_user_metric, tweet_count my_user_metric, followers_count my_user_metric)
    else do
      --Update user
      print "User Already exists, Updataing..."
      executeNamed conn "UPDATE users SET username = :username, name= :name, verified = :verified, location = :location, bio = :bio, following_count = :following_count, tweet_count= :tweet_count, followers_count = :followers_count WHERE user_id = :user_id" [":username" := username my_user, ":name" := name my_user, ":verified" := verified my_user, ":location" := location my_user, ":bio" := bio my_user, ":following_count" := following_count my_user_metric, ":tweet_count" := tweet_count my_user_metric, ":followers_count" := followers_count my_user_metric, ":user_id" := user_id my_user]

-- ##########################################################################################################################################

-- |
--
--  Checks whether the given tweet exists in the tweets table using the tweet_id.
--  Inserts an entry if it does exsts. Updates an entry if it does not.
--  Prepared statements used for additonal security.
saveTweet ::
  -- | Connection to sqlite database
  Connection ->
  -- | Search string used to return the tweet from the twitter api. Set to '' if the user accessed the tweet using its tweet_id.
  [Char] ->
  -- | Tweet record object. tweet_id returned using tweet_id Tweet
  Tweet ->
  -- | Used to print success or failure messages to the user
  IO ()
saveTweet conn input my_tweet = do
  --print (dynTypeRep (toDyn my_tweet))
  let my_tweet_id = tweet_id my_tweet
  let my_tweet_metric = tweet_metrics my_tweet
  let my_user_id = fk_user_id my_tweet
  --print my_tweet_id
  -- Check if a Tweet already exists
  checkTweet <- queryNamed conn "SELECT * FROM tweets WHERE tweet_id = :tweet_id" [":tweet_id" := my_tweet_id] :: IO [TweetField]
  --print $ length checkTweet
  if null checkTweet
    then do
      -- Add User to Database
      json <- getUserByID my_user_id --fetch user information
      case parseDataUser json of
        Left err -> print err
        Right result -> do
          let output = raw_user_data result
          let metrics = user_metrics output
          let output_metrics = UserMetrics (following_count metrics) (tweet_count metrics) (followers_count metrics)
          let output_user = User (user_id output) (username output) (name output) (verified output) (location output) (created_at output) (bio output) output_metrics

          -- Add user to database
          saveUser conn output_user
          -- Add Tweet to Database
          print "Inserting new tweets..."
          execute conn "INSERT INTO tweets (tweet_id,fk_user_id,tweeted_at, contents, quote_count, retweet_count, like_count, reply_count, search_query) VALUES (?,?, ?, ?, ?, ?, ?, ?, ?)" (tweet_id my_tweet, fk_user_id my_tweet, tweeted_at my_tweet, contents my_tweet, quote_count my_tweet_metric, retweet_count my_tweet_metric, like_count my_tweet_metric, reply_count my_tweet_metric, input)
    else do
      --Update Tweet
      print "Tweet Already exists, Updating..."
      executeNamed conn "UPDATE tweets SET like_count = :like_count, retweet_count=:retweet_count, quote_count=:quote_count , reply_count=:reply_count  WHERE tweet_id = :tweet_id" [":tweet_id" := tweet_id my_tweet, ":like_count" := like_count my_tweet_metric, ":retweet_count" := retweet_count my_tweet_metric, ":quote_count" := quote_count my_tweet_metric, ":reply_count" := reply_count my_tweet_metric]

-- ##########################################################################################################################################

-- | Inserts/updates multiple entries in tweets by calling saveTweet for each tweet in the given list.
saveTweets ::
  -- | Connection to sqlite database
  Connection ->
  -- | Search query used to get tweets
  [Char] ->
  -- | List of tweets
  [Tweet] ->
  -- | Used for returning any results
  IO ()
saveTweets conn input tweets = mapM_ (saveTweet conn input) tweets

-- ##########################################################################################################################################

querySavedUsers :: Connection -> IO ()
querySavedUsers conn = do
  putStr "Enter username > "
  userName <- getLine
  putStrLn $ "Looking for " ++ userName ++ "..."
  searchUser <- queryNamed conn "SELECT * FROM users WHERE username = :username" [":username" := map Data.Char.toLower userName] :: IO [UserField]
  if not $ null searchUser
    then do
      print "Found user in Database!"
      print $ head searchUser
    else do
      print "User not found in Database!"

querySavedTweetsByUser :: Connection -> IO ()
querySavedTweetsByUser conn = do
  putStr "Enter username > "
  userName <- getLine
  putStrLn $ "Looking for " ++ userName ++ "'s Tweets..."
  uTweets <- queryNamed conn "SELECT tweets.* FROM tweets INNER JOIN users ON tweets.fk_user_id == users.user_id WHERE username = :username" [":username" := userName] :: IO [TweetField]
  if not $ null uTweets
    then do
      print $ head uTweets
    else do
      print "No tweets found!"

-- ##########################################################################################################################################

-- For downloading

-- |
--
--  Downloads users from the users table to users.json.

-- | 1. A select query is executed to get users
--
--  2. The results are encoded to JSON
-- 3. The JSON is written to user.json
queryUsers ::
  -- | Connection to sqlite database
  Connection ->
  -- | Write json to file
  IO ()
queryUsers conn = do
  getUsers <- query_ conn "SELECT * FROM users" :: IO [Userdb]
  print getUsers
  let results = DAT.encodeToLazyText getUsers
  --print results
  I.writeFile "users.json" results

-- |
--
--  Downloads users from the tweets table to tweets.json.

-- | 1. A select query is executed to get tweets
--
--  2. The results are encoded to JSON
-- 3. The JSON is written to tweets.json
queryTweets ::
  -- | Connection to sqlite database
  Connection ->
  -- | Write json to file
  IO ()
queryTweets conn = do
  getTweets <- query_ conn "SELECT * FROM tweets" :: IO [Tweetdb]
  print getTweets
  let results = DAT.encodeToLazyText getTweets
  --print results
  I.writeFile "tweets.json" results

-- |
--
--  Downloads users and tweets from the database to tweets_users.json.

-- | 1. A select query is executed to get tweets which are left joined to users on tweets.fk_user_id = users.user_id
--
--  2. The results are encoded to JSON
-- 3. The JSON is written to tweets_users.json
queryTweetsUsers :: Connection -> IO ()
queryTweetsUsers conn = do
  getTweets <- query_ conn "SELECT t.*, u.* from tweets as t LEFT JOIN users as u ON t.fk_user_id = u.user_id" :: IO [TweetUserdb]
  --print getTweets
  let results = DAT.encodeToLazyText getTweets
  --print results
  I.writeFile "tweets_users.json" results