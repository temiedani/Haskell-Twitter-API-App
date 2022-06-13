{-# LANGUAGE DeriveGeneric #-}

-- |
--  Module      : Types.hs
--  Description : Used to convert return data from twitter and data in sqlite into Haskell record types.
--
--  This module is used convert return data from twitter and data in sqlite into Haskell record types.
module Types
  ( Error (..),
    RawUser (..),
    User (..),
    Users (..),
    UserMetrics (..),
    RawTweets (..),
    Tweet (..),
    Tweets (..),
    TweetMetrics (..),
    -- For downloading
    Userdb (..),
    Tweetdb (..),
    TweetUserdb (..),
  )
where

import Data.Aeson
import GHC.Generics (Generic)

-- ##########################################################################################################################################

-- |
--
--  Used for parse errors
data Error = Error
  { error_data :: Maybe [Object],
    meta_error :: Maybe Object
  }
  deriving (Eq, Show, Generic)

-- |
--
--  Used when parsing.
--  User data returned from twitter api is stored in the User type (found below) using this type.
newtype RawUser = RawUser
  { raw_user_data :: User
  }
  deriving (Eq, Show, Generic)

-- |
--
--  User Record object corresponding to the user data returned by the twitter api.
--  Similarly to the twitter api, user_metrics are stored in a nested object.
--  This type is used to store the result of twitter api requests (after being parsed) and to insert data into the database
data User = User
  { user_id :: String,
    username :: String,
    name :: String,
    verified :: Maybe Bool,
    location :: Maybe String,
    created_at :: String,
    bio :: Maybe String,
    user_metrics :: UserMetrics
  }
  deriving (Eq, Show, Generic)

-- | Stores user_metrics
data UserMetrics = UserMetrics
  { following_count :: Int,
    tweet_count :: Int,
    followers_count :: Int
  }
  deriving (Eq, Show, Generic)

-- |
--
--  Users are simply stored as a list of User which are defined above
newtype Users = Users
  { users :: [User]
  }
  deriving (Eq, Show, Generic)

-- ##########################################################################################################################################

newtype RawTweets = RawTweets
  { raw_tweet_data :: Tweet
  }
  deriving (Eq, Show, Generic)

-- |
--
--  Tweet Record object corresponding to the tweet data returned by the twitter api.
--  Similarly to the twitter api, tweet_metrics are stored in a nested object.
--  This type is used to store the result of twitter api requests (after being parsed) and to insert data into the database
data Tweet = Tweet
  { tweet_id :: String,
    fk_user_id :: String,
    tweeted_at :: Maybe String,
    contents :: Maybe String,
    tweet_metrics :: TweetMetrics
  }
  deriving (Eq, Show, Generic)

-- | Stores user_metrics
data TweetMetrics = TweetMetrics
  { quote_count :: Int,
    retweet_count :: Int,
    like_count :: Int,
    reply_count :: Int
  }
  deriving (Eq, Show, Generic)

-- |
--
--  Tweets are simply stored as a list of Tweet which are defined above
newtype Tweets = Tweets
  { tweets :: [Tweet]
  }
  deriving (Eq, Show, Generic)

-- ##########################################################################################################################################

-- |
--
--  User Record object corresponding to the user data in the users table.
--  Unlike the twitter api there are no nested objects and 1:1 mappings between the record fields and database columns.
--  This type is used when running select queries on the database.
data Userdb = Userdb
  { db_user_id :: String,
    db_username :: String,
    db_name :: String,
    db_verified :: Maybe Bool,
    db_location :: Maybe String,
    db_created_at :: String,
    db_bio :: Maybe String,
    db_following_count :: Int,
    db_tweet_count :: Int,
    db_followers_count :: Int
  }
  deriving (Eq, Show, Generic)

-- |
--
--  Tweet Record object corresponding to the tweet data in the tweets table.
--  Unlike the twitter api there are no nested objects and 1:1 mappings between the record fields and database columns.
--  This type is used when running select queries on the database.
data Tweetdb = Tweetdb
  { db_tweet_id :: String,
    db_fk_user_id :: String,
    db_tweeted_at :: Maybe String,
    db_contents :: Maybe String,
    db_quote_count :: Int,
    db_retweet_count :: Int,
    db_like_count :: Int,
    db_reply_count :: Int,
    db_search_query :: Maybe String
  }
  deriving (Eq, Show, Generic)

-- |
--
--  Contains the data for both tweets and users in the database with 1:1 mapppings between record fields and database columns.
--  This object is used when selecting tweets from the database and joining the tweets with the corresponding users.
data TweetUserdb = TweetUserdb
  { db_tweet_id' :: String,
    db_fk_user_id' :: String,
    db_tweeted_at' :: Maybe String,
    db_contents' :: Maybe String,
    db_quote_count' :: Int,
    db_retweet_count' :: Int,
    db_like_count' :: Int,
    db_reply_count' :: Int,
    db_search_query' :: Maybe String,
    db_user_id' :: String,
    db_username' :: String,
    db_name' :: String,
    db_verified' :: Maybe Bool,
    db_location' :: Maybe String,
    db_created_at' :: String,
    db_bio' :: Maybe String,
    db_following_count' :: Int,
    db_tweet_count' :: Int,
    db_followers_count' :: Int
  }
  deriving (Eq, Show, Generic)