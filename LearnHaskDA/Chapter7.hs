{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module LearnHaskDA.Chapter7 where
import Data.List as L
import Data.Hashable
import Data.HashMap.Strict as HM
import Database.HDBC.Sqlite3
import Database.HDBC
import Control.Concurrent
import Data.Char
import Network.HTTP.Conduit
import Web.Authenticate.OAuth
import Data.Aeson
import GHC.Generics
import qualified Control.Exception as E

-- | The constructor names come from twitter JSON format:
--    "screen_name", "statuses" (tweets), "text", "lang", "user", ...

data User = User { screen_name :: !String }
  deriving (Show, Generic)

data Tweet = Tweet  { text :: !String
                    , lang :: !String
                    , user :: !User }
  deriving (Show, Generic)

data Search = Search { statuses :: ![Tweet] }
  deriving (Show, Generic)

instance FromJSON User
instance FromJSON Tweet
instance FromJSON Search

instance ToJSON   User
instance ToJSON   Tweet
instance ToJSON   Search

loadConfig :: IO (OAuth, Credential)
loadConfig = do
  myoauth <- read <$> readFile "/home/patrikj/secret/twitter_oauth.hs"
  mycred  <- read <$> readFile "/home/patrikj/secret/twitter_cred.hs"
  return (myoauth, mycred)

type E = Either String

twitterSearch :: String -> IO (E Search)
twitterSearch term = do
  (myoauth, mycred) <- loadConfig
  req <- parseUrl $ -- https://api.twitter.com/1.1/search/tweets.json?q
     "https://api.twitter.com/1.1/search/tweets.json?count=100&q=" ++ term
  -- Using a HTTP manager, we authenticate the request and send it to get a response.
  manager <- newManager tlsManagerSettings
  -- OAuth Authentication.
  signedreq <- signOAuth myoauth mycred req
  -- Send request and Decode the response body.
  fmap (eitherDecode . responseBody) (httpLbs signedreq manager)
    `E.catch` handler

-- I had problems with failed connection exceptions.
handler e@(FailedConnectionException2 {}) = return (Left (show e))
handler e = return (Left (show e)) -- should not be this generic, but I don't have the patience to list the cases I ran into


createTweetsDatabase :: IO ()
createTweetsDatabase = do
    conn <- connectSqlite3 "tweets.sql"
    run conn createStatement []
    commit conn
    disconnect conn
    putStrLn "Successfully created database."
  where
    createStatement = "CREATE TABLE tweets (message TEXT, user TEXT, language TEXT)"


insertTweetsInDatabase :: [Tweet] -> IO ()
insertTweetsInDatabase tweets = do
    conn <- connectSqlite3 "tweets.sql"
    stmt <- prepare conn insertStatement
    executeMany stmt sqlRecords
    commit conn
    disconnect conn
    putStrLn "Successfully inserted Tweets to database."
  where
    insertStatement = "INSERT INTO tweets VALUES (?, ?, ?)"
    sqlRecords = L.map (\(Tweet message language (User user)) ->
                          [toSql message, toSql user, toSql language])
                       tweets

collectTweetsIntoDatabase :: IO ()
collectTweetsIntoDatabase = do
    status <- twitterSearch "a"
    either  putStrLn  (insertTweetsInDatabase . statuses)  status
    threadDelay 5000

slurpTweets = sequence_ (replicate 180 collectTweetsIntoDatabase)
