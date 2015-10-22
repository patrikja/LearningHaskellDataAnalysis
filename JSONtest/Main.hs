-- From https://www.fpcomplete.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
{-# LANGUAGE OverloadedStrings, DeriveGeneric, StandaloneDeriving #-}
module Main where
import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Network.HTTP.Conduit (simpleHttp, withManager, parseUrl, httpLbs, responseBody,
                             newManager, tlsManagerSettings)
import Web.Authenticate.OAuth
import qualified Data.ByteString.Lazy as B

-- | Type of each JSON entry in record syntax.
data Person =
  Person { firstName  :: !Text
         , lastName   :: !Text
         , age        :: Int
         , likesPizza :: Bool
           } deriving (Show,Generic)

-- Instances to convert our type to/from JSON.

instance FromJSON Person
instance ToJSON Person

-- | Location of the local copy, in case you have it,
--   of the JSON file.
jsonFile :: FilePath
jsonFile = "pizza.json"

-- | URL that points to the remote JSON file, in case
--   you have it.
jsonURL :: String
jsonURL = "http://daniel-diaz.github.io/misc/pizza.json"

-- Move the right brace (}) from one comment to another
-- to switch from local to remote.

{--
-- Read the local copy of the JSON file.
getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile
--}

{--}
-- Read the remote copy of the JSON file.
getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL
--}

main :: IO ()
main = do
 -- Get JSON data and decode it
 d <- (eitherDecode <$> getJSON) :: IO (Either String [Person])
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
  Left err -> putStrLn err
  Right ps -> print ps

{-
-- Storing the keys in the (open) source file would be a bad idea.
myoauth :: OAuth
myoauth =
  newOAuth { oauthServerName     = "api.twitter.com"
           , oauthConsumerKey    = "your consumer key here"
           , oauthConsumerSecret = "your consumer secret here"
           }

mycred :: Credential
mycred = newCredential "your access token here"
                       "your access token secret here"
-}

{- A failed attempt at storing the keys using JSON - OAuth does not export all its constructors.
deriving instance Generic OAuth
deriving instance Generic Credential
instance FromJSON OAuth
instance ToJSON OAuth
instance FromJSON Credential
instance ToJSON Credential

type E = Either String
loadConfig :: IO (E OAuth, E Credential)
loadConfig = do
  myoauth <- eitherDecode <$> B.readFile "/home/patrikj/secret/twitter_oauth.json"
  mycred  <- eitherDecode <$> B.readFile "/home/patrikj/secret/twitter_cred.json"
  return (myoauth, mycred)
-}

loadConfig :: IO (OAuth, Credential)
loadConfig = do
  myoauth <- read <$> readFile "/home/patrikj/secret/twitter_oauth.hs"
  mycred  <- read <$> readFile "/home/patrikj/secret/twitter_cred.hs"
  return (myoauth, mycred)

data Tweet =
  Tweet { text :: !Text
        , created_at :: !UTCTime
          } deriving (Show, Generic)

instance FromJSON Tweet
instance ToJSON Tweet

timeline :: String -- ^ Screen name of the user
         -> IO (Either String [Tweet]) -- ^ If there is any error parsing the JSON data, it
                                       --   will return 'Left String', where the 'String'
                                       --   contains the error information.
timeline name = do
  (myoauth, mycred) <- loadConfig
  -- Firstly, we create a HTTP request with method GET (it is the default so we don't have to change that).
  req <- parseUrl $ "https://api.twitter.com/1.1/statuses/user_timeline.json?screen_name=" ++ name
  -- Using a HTTP manager, we authenticate the request and send it to get a response.
  manager <- newManager tlsManagerSettings
  -- OAuth Authentication.
  signedreq <- signOAuth myoauth mycred req
  -- Send request.
  res <- httpLbs signedreq manager
  -- Decode the response body.
  return $ eitherDecode $ responseBody res
