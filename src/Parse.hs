{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Parse
  ( parseDataUser,
    parseUser,
    parseUserMetrics,
    parseDataTweet,
    parseTweet,
    parseTweets,
    parseTweetMetrics,
    parseError,
  )
where

import Control.Arrow
import Data.Aeson
import qualified Data.Aeson as JSON
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import qualified Distribution.Fields as JSON
import Distribution.SPDX (LicenseId (JSON))
import Distribution.Simple (License (BSD2))
import GHC.Generics
import Types

-- ##########################################################################################################################################
--  Renames the field in the response JSON to match the variable name defined in types
renameFieldsUser :: [Char] -> [Char]
renameFieldsUser "error_data" = "errors"
renameFieldsUser "meta_error" = "meta"
renameFieldsUser "raw_user_data" = "data"
renameFieldsUser "user_id" = "id"
renameFieldsUser "bio" = "description"
renameFieldsUser "user_metrics" = "public_metrics"
renameFieldsUser other = other

-- apply the above field label modiefier as option
customOptionsUser :: Options
customOptionsUser =
  defaultOptions
    { fieldLabelModifier = renameFieldsUser
    }

-- Parse Error
instance FromJSON Error where
  parseJSON = JSON.genericParseJSON customOptionsUser

-- |
--
--  Parses the JSON file in case an error response is retuened by the function
parseError :: L8.ByteString -> Either String Error
parseError json = eitherDecode json :: Either String Error

-- Pasrsing Raw User Data
instance FromJSON RawUser where
  parseJSON = JSON.genericParseJSON customOptionsUser

-- |
--
--  Parses the raw data returned by the getUser funtion in fetch
parseDataUser :: L8.ByteString -> Either String RawUser
parseDataUser json = eitherDecode json :: Either String RawUser

-- Parsing User Data
instance FromJSON User where
  parseJSON = JSON.genericParseJSON customOptionsUser

-- |
--
--  Parse a single user by encodiing Json to User data Type.
parseUser :: L8.ByteString -> Either String User
parseUser json = eitherDecode json :: Either String User

-- Parsing User Metrics Data
instance FromJSON UserMetrics where
  parseJSON = withObject "public_metrics" $ \b ->
    UserMetrics <$> b .: "following_count"
      <*> b .: "tweet_count"
      <*> b .: "followers_count"

-- |
--
--  Renames the field in the response JSON to match the variable name defined in types
parseUserMetrics :: L8.ByteString -> Either String UserMetrics
parseUserMetrics json = eitherDecode json :: Either String UserMetrics

-- ##########################################################################################################################################
-- Remove Extra header from twitter API in some instances
jsonOptions :: String -> JSON.Options
jsonOptions prefix =
  let prefixLength = length prefix
   in JSON.defaultOptions {JSON.fieldLabelModifier = drop prefixLength >>> renameFieldsUser}

-- ##########################################################################################################################################
-- rename some of the fields in the Json
renameFieldsRaw "raw_tweet_data" = "data"
renameFieldsRaw "tweeted_at" = "created_at"
renameFieldsRaw other = other

customOptionsRaw :: Options
customOptionsRaw =
  defaultOptions
    { fieldLabelModifier = renameFieldsRaw
    }

-- Parsing Raw Tweet Data
instance FromJSON RawTweets where
  parseJSON = JSON.genericParseJSON customOptionsRaw

--  Parses the raw data returned by the getUser funtion in fetch defined in fetch and used in main
parseDataTweet :: L8.ByteString -> Either String RawTweets
parseDataTweet json = eitherDecode json :: Either String RawTweets

renameFieldsTweet :: [Char] -> [Char]
renameFieldsTweet "tweets" = "data"
renameFieldsTweet "tweet_id" = "id"
renameFieldsTweet "tweeted_at" = "created_at"
renameFieldsTweet "fk_user_id" = "author_id"
renameFieldsTweet "contents" = "text"
renameFieldsTweet "tweet_metrics" = "public_metrics"
renameFieldsTweet other = other

customOptionsTweet :: Options
customOptionsTweet =
  defaultOptions
    { fieldLabelModifier = renameFieldsTweet
    }

-- Parsing  a single Tweet Data
instance FromJSON Tweet where
  parseJSON = JSON.genericParseJSON customOptionsTweet

-- |
--
--  Parse a single tweet by decoding from Json to Tweet that is returned by GetTweet function defiend in fetch and used in main
parseTweet :: L8.ByteString -> Either String Tweet
parseTweet json = eitherDecode json :: Either String Tweet

-- Parsing Array of Tweet Data
instance FromJSON Tweets where
  parseJSON = JSON.genericParseJSON customOptionsTweet

-- |
--
--  Parses the arrays of Tweets by decoding from Json to Tweeets returned by SearchTweets function defiend in fetch and used in main
parseTweets :: L8.ByteString -> Either String Tweets
parseTweets json = eitherDecode json :: Either String Tweets

-- Parsing Tweet Metrics Data
instance FromJSON TweetMetrics where
  parseJSON = JSON.genericParseJSON customOptionsTweet

-- |
--
--  Parses the Tweet metrics by decoding from Json to TweetMetrics type
parseTweetMetrics :: L8.ByteString -> Either String TweetMetrics
parseTweetMetrics json = eitherDecode json :: Either String TweetMetrics

-- ##########################################################################################################################################
