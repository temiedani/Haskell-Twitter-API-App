module Main where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Dynamic
import qualified Data.Text as DT
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml.Builder (toByteString)
import Database
import Distribution.SPDX (LicenseId (JSON))
import Distribution.Types.InstalledPackageInfo.Lens (description)
import Fetch
import Parse
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Types

packStr'' :: String -> BS.ByteString
packStr'' = encodeUtf8 . DT.pack

main = do
  putStrLn "---------------------------------"
  putStrLn "  Welcome to the Twitter API app "
  putStrLn "  (1) Get user information       "
  putStrLn "  (2) Search tweets by Topic     "
  putStrLn "  (3) Get tweet by ID            "
  putStrLn "  (4) Get user by ID             "
  putStrLn "  (5) Search saved users         "
  putStrLn "  (6) Search saved user's tweets "
  putStrLn "  (7) Export database to JSON    "
  putStrLn "  (8) Quit                       "
  putStrLn "---------------------------------"
  conn <- initDB
  hSetBuffering stdout NoBuffering
  putStr "Choose an option > "
  option <- readLn :: IO Int
  case option of
    1 -> do
      putStr "Twitter username: "
      hFlush stdout
      input <- getLine
      print "Downloading..."
      json <- getUser input
      print "Parsing..."
      case parseDataUser json of
        Left err -> case parseError json of
          Left err -> print err
          Right errorresult -> do
            print "Could not find user with such username"
            main
        Right result -> do
          let output_user = raw_user_data result
          print output_user
          print "Saving on DB..."
          saveUser conn output_user
          main
    2 -> do
      putStr "Search term: "
      hFlush stdout
      input <- getLine
      print "Downloading..."
      json <- searchTweets input
      print "Parsing..."
      case parseTweets json of
        Left err -> case parseError json of
          Left err -> print err
          Right errorresult -> do
            print "Could not any result"
            main
        Right result -> do
          let output_tweets = tweets result
          print "Saving on DB..."
          saveTweets conn input output_tweets -- also inserts input into search_query column
          main
    3 -> do
      putStr "Tweet ID: "
      hFlush stdout
      input <- getLine
      print "Downloading..."
      json <- getTweet input
      print "Parsing..."
      case parseDataTweet json of
        Left err -> case parseError json of
          Left err -> print err
          Right errorresult -> do
            print "Could not find tweet with such ID"
            main
        Right result -> do
          let output_tweet = raw_tweet_data result
          print output_tweet
          print "Saving on DB..."
          saveTweet conn input output_tweet
          main
    4 -> do
      putStr "User ID: "
      hFlush stdout
      input <- getLine
      print "Downloading..."
      json <- getUserByID input
      print "Parsing..."
      case parseDataUser json of
        Left err -> case parseError json of
          Left err -> print err
          Right errorresult -> do
            print "Could not find user with such ID"
            main
        Right result -> do
          let output = raw_user_data result
          let metrics = user_metrics output
          let output_metrics = UserMetrics (following_count metrics) (tweet_count metrics) (followers_count metrics)
          let output_user = User (user_id output) (username output) (name output) (verified output) (location output) (created_at output) (bio output) output_metrics
          print output_user
          print "Saving on DB..."
          saveUser conn output_user
          main
    5 -> do
      querySavedUsers conn
      main
    6 -> do
      querySavedTweetsByUser conn
      main
    7 -> do
      putStrLn "----------------------------------------------"
      putStrLn "  Please choose an option                     "
      putStrLn "  (1) Download user data                      "
      putStrLn "  (2) Download tweet data                     "
      putStrLn "  (3) Download tweet data joined to user data "
      putStrLn "----------------------------------------------"
      putStr "Choose an option > "
      downloadOption <- readLn :: IO Int

      case downloadOption of
        1 -> do
          print "Saving user data to user.json"
          queryUsers conn
          main
        2 -> do
          print "Saving tweet data to tweets.json"
          queryTweets conn
          main
        3 -> do
          print "Saving user and tweet data to tweets_users.json"
          queryTweetsUsers conn
          main
        _ -> do
          exitFailure
    8 -> do
      exitSuccess
    _ -> do
      exitFailure
