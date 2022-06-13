{-# LANGUAGE OverloadedStrings #-}

-- |
--  Module      : Fetch.hs
--  Description : Used for performing HTTP GET requests to the twitter api
--
--  This module is used for performing HTTP GET requests to the twitter api.
--  It has the following functionality:
--
--  1. Validating user input and handling HTTP exceptions
--
--  2. Performing GET requests to search tweets, find a user by id/username, find a tweet by id
--
--   3. Connecting to the twitter api using a bearer token.
module Fetch (getTweet, getUser, searchTweets, getUserByID, isNumeric, containsSymbol) where

import Control.Exception (try)
import Data.Aeson (FromJSON, Value, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as S8
import Data.Char (isDigit, isLetter)
import qualified Data.Text as DT
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple
import System.Exit (exitFailure)

-- ##########################################################################################################################################

-- |
--  Checks if a string contains only numeric characters
isNumeric ::
  -- | User input
  [Char] ->
  -- | True if the user enters only characters which are numbers and False otherwise
  Bool
isNumeric [] = True
isNumeric (a : as) = if (isDigit a) then isNumeric as else False

-- |
--  Used to ensure the user enters a correct twitter username
containsSymbol ::
  -- | Twitter username
  [Char] ->
  -- | Returns True if a valid usernam i.e. one that contains only numbers, letters or _. Returns False otherwise.
  Bool
containsSymbol [] = False
containsSymbol (a : as) = if not (isDigit a) && not (isLetter a) && a /= '_' then True else containsSymbol as

-- |
--  Bearer token used to connect to the Twitter API
bearerToken ::
  -- | Bearer Token
  ByteString
bearerToken = "Bearer AAAAAAAAAAAAAAAAAAAAAKqQWAEAAAAADjJIsHNmXPfaCE8vNJkEZbL8z%2Fw%3DeoJJbBerLBYwlS9TeLEQxfK9XAlzZ801y7oHsuU0jAzInIBCnI"

-- ##########################################################################################################################################

-- |
--
--  Returns tweets matching the validated search criteria given by the user.

-- | 1. Validates user input by checking if the user enters a search term
--
--  2. URL encodes # to allow users to search twitter tags
-- 3. Runs the search query by calling a GET request to the twitter api and attempts to return tweets matching the search query
-- 4. The case of a search not returning tweets is handled in main and parse by checking if the returned json contains any errors
searchTweets ::
  -- | search_query given by the user
  [Char] ->
  -- | Returns 'Error' if validation fails. Otherwise returns any tweets matching the search query.
  IO S8.ByteString
searchTweets query = do
  case query of
    "" -> do
      Prelude.putStrLn "Please enter a search term"
      exitFailure
      return "Error"
    otherwise -> do
      let first = Prelude.head query
      let first' = case first of
            '#' -> "%23"
            otherwise -> [first]
      let urlQuery = first' ++ Prelude.tail query
      let filters = " lang:en &expansions=author_id&tweet.fields=public_metrics,created_at" --Tweets in English. Expansions show more information in results.
      let extension = urlQuery ++ filters
      request' <- parseRequest $ "GET https://api.twitter.com/2/tweets/search/recent?query=" ++ extension
      let request =
            addRequestHeader "Authorization" bearerToken $
              --setRequestQueryString [("tweet.fields", Just "author_id,created_at,public_metrics")] $
              setRequestSecure True $
                setRequestPort
                  443
                  request'
      response <- httpLBS request
      --S8.putStrLn $ encode (getResponseBody response :: S8.ByteString)
      -- print $ getRequestQueryString request --debug only
      let statusCode = getResponseStatusCode response
      case statusCode of
        200 -> do
          return $ getResponseBody response
        otherwise -> do
          Prelude.putStrLn "Invalid request"
          exitFailure
          return "Error"

-- ##########################################################################################################################################

-- |
--
--  Returns a tweet matching the validated tweet id given by the user.

-- | 1. Validates user input by checking if the user enters a tweet_id and that it contains only numbers
--
--  2. Calls a GET request to the twitter api and attempts to return a tweet matching the tweet_id
-- 3. The case of a search not returning a tweet is handled in main and parse by checking if the returned json contains any errors
getTweet ::
  -- | tweet_id given by user
  [Char] ->
  -- | Returns 'Error' if validation fails or there is another exception. Otherwise returns the tweet found by the twitter api.
  IO S8.ByteString
getTweet tweetId = do
  let tweetId' = if isNumeric tweetId then tweetId else "Not numeric"
  case tweetId' of
    "" -> do
      print "Please enter a tweet ID"
      return "Error"
    "Not numeric" -> do
      print "Please enter a numeric tweet ID"
      exitFailure
      return "Error"
    otherwise -> do
      --let filters = "?tweet.fields=author_id,created_at,public_metrics" --Tweets in English. Expansions show more information in results.
      --let extension = tweetId ++ filters
      request' <- parseRequest $ "GET https://api.twitter.com/2/tweets/" ++ tweetId
      let request =
            addRequestHeader "Authorization" bearerToken $
              setRequestQueryString [("tweet.fields", Just "author_id,created_at,public_metrics")] $
                setRequestSecure True $
                  setRequestPort
                    443
                    request'
      response <- httpLBS request
      --S8.putStrLn $ encode (getResponseBody response :: S8.ByteString )
      --print $ getRequestQueryString request --debug only
      let statusCode = getResponseStatusCode response
      case statusCode of
        200 -> do
          return $ getResponseBody response
        400 -> do
          Prelude.putStrLn "Invalid request"
          exitFailure
          return "Error"
        _ -> do
          Prelude.putStrLn "An unknown error has occured"
          exitFailure
          return "Error"

-- ##########################################################################################################################################

-- |
--
--  Returns a twitter user matching the validated username entered by the user.

-- | 1. Validates user input by checking if the user enters a username, it contains only numbers/letters/underscors, it has a length between 4 and 15 characters.
-- This is based on the rules defined at: https://help.twitter.com/en/managing-your-account/twitter-username-rules#:~:text=A%20username%20can%20only%20contain,a%20suspended%20or%20deactivated%20account.
--
--  2. Calls a GET request to the twitter api and attempts to return a user matching the username
-- 3. The case of the request not returning a tweet is handled in main and parse by checking if the returned json contains any errors
getUser ::
  -- | username given by user
  [Char] ->
  -- | Returns 'Error' if validation fails or their is an issue running the HTTP request. Otherwise returns the user found by the twitter api
  IO S8.ByteString
getUser username = do
  let length' = Prelude.length username
  let username'
        | containsSymbol username = "Invalid username"
        | (length' > 15 || length' < 4) = "Invalid length"
        | otherwise = username
  --Validate user input
  case username' of
    "Invalid username" -> do
      Prelude.putStrLn "Please enter a useraname containing only letters, numbers, or _"
      exitFailure
      return "Error"
    "Invalid length" -> do
      Prelude.putStrLn "Please enter a useraname between 4 and 15 characters"
      exitFailure
      return "Error"
    otherwise -> do
      Prelude.putStrLn "Valid username"
      request' <- parseRequest $ "GET https://api.twitter.com/2/users/by/username/" ++ username

      let request =
            addRequestHeader "Authorization" bearerToken $
              setRequestSecure True $
                setRequestQueryString [("user.fields", Just "public_metrics,created_at,location,description,verified")] $
                  setRequestPort
                    443
                    request'
      response <- try $ httpLBS request

      --Ensure request can be run correctly
      case response of
        Left e -> do
          --catch connection errors
          print (e :: HttpException)
          Prelude.putStrLn "Please ensure you are connected to the internet"
          exitFailure
          return "Error"
        Right response -> do
          --S8.putStrLn $ (getResponseBody response) --debug only
          --print $ getRequestQueryString request --debug only
          --print $ getResponseStatusCode response --debug only
          let statusCode = getResponseStatusCode response
          case statusCode of
            200 -> do
              return $ getResponseBody response
            otherwise -> do
              Prelude.putStrLn "Bad response, Invalid request"
              exitFailure
              return "Error"

-- ##########################################################################################################################################

-- |
--
--  Returns a twitter user matching the validated user_id entered by the user.

-- | 1. Validates user input by checking if the user enters a user_id, it contains only numbers/letters/underscors, it has a length less than 20 characters.
--
--  2. Calls a GET request to the twitter api and attempts to return a user matching the user_id
-- 3. The case of the request not returning a user is handled in main and parse by checking if the returned json contains any errors
getUserByID ::
  -- | user_id entered by the user
  [Char] ->
  -- | Returns 'Error' if validation fails or their is an issue with the HTTP request. Returns the twitter user matching the given user_id otherwise.
  IO S8.ByteString
getUserByID id = do
  let length' = Prelude.length id
  let id'
        | not (isNumeric id) = "Invalid user id"
        | (length' > 20 || length' < 1) = "Invalid length"
        | otherwise = id
  --Validate user input
  case id' of
    "Invalid user id" -> do
      Prelude.putStrLn "Please enter a user id containing only of numbers"
      exitFailure
      return "Error"
    "Invalid length" -> do
      Prelude.putStrLn "Please enter an id between 1 and 20 characters"
      exitFailure
      return "Error"
    otherwise -> do
      --Prelude.putStrLn "Valid user id"
      request' <- parseRequest $ "GET https://api.twitter.com/2/users/" ++ id

      let request =
            addRequestHeader "Authorization" bearerToken $
              setRequestSecure True $
                setRequestQueryString [("user.fields", Just "public_metrics,created_at,location,description,verified")] $
                  setRequestPort
                    443
                    request'
      response <- try $ httpLBS request
      --Ensure request can be run correctly
      case response of
        Left e -> do
          --catch connection errors
          print (e :: HttpException)
          Prelude.putStrLn "Please ensure you are connected to the internet"
          exitFailure
          return "Error"
        Right response -> do
          --S8.putStrLn $ (getResponseBody response)
          --print $ getRequestQueryString request --debug only
          --print $ getResponseStatusCode response --debug only
          let statusCode = getResponseStatusCode response
          case statusCode of
            200 -> do
              return $ getResponseBody response
            otherwise -> do
              Prelude.putStrLn "Invalid request, Bad request"
              exitFailure
              return "Error"

-- ##########################################################################################################################################
