{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module LearnHaskDA.Chapter7 where
import LearnHaskDA.Chapter4 (queryDatabase)
import Data.List as L
import Data.Ord (comparing)
import Data.Hashable
import Data.HashMap.Strict as HM
import Data.Convertible.Base
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

-- instance ToJSON   User
-- instance ToJSON   Tweet
-- instance ToJSON   Search

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
-- The delay is supposed to make sure we don't end up over the free
-- twitter api quota (but I think something wrong with the delay).

slurpTweets = sequence_ (replicate 180 collectTweetsIntoDatabase)

fromSqlPair ::  (Convertible SqlValue a, Convertible SqlValue b) =>
                [SqlValue] -> (a, b)
fromSqlPair (a:b:_) = (fromSql a, fromSql b)

test125 :: IO [(String, String)]
test125 = do
  sqlTweets <- queryDatabase "tweets.sql" "SELECT message, language FROM tweets"
  return (L.map fromSqlPair sqlTweets)

frequency ::  (Eq k, Hashable k, Integral v) =>
              [k] -> HashMap k v
frequency = fromListWith (+) . L.map (\k -> (k, 1))
-- fromListWith :: (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> HashMap k v


type MyWord = String
removePunctuation :: MyWord -> MyWord
removePunctuation = L.map (toLower) . L.filter isAlpha

-- | Clean removes @ replies, hashtags, links and punctuation from strings.
clean :: String -> [MyWord]
clean = L.map removePunctuation
      . L.filter (\myWord -> not (or
                    [ isInfixOf "@"        myWord
                    , isInfixOf "#"        myWord
                    , isInfixOf "http://"  myWord
                    , isInfixOf "https://" myWord]))
      . words

test126 = do
  tweets <- test125
  let freqTable = frequency tweets
  let uniqueTweets = HM.keys freqTable
--  HM.size freqTable
  let cleanedTweets = zip (L.map snd uniqueTweets) (L.map (clean.fst) uniqueTweets)
  let languageFrequency = (frequency . L.map fst) cleanedTweets
  return languageFrequency

-- let allLanguages = HM.keys languageFrequency
-- let wordFrequency = (frequency . concatMap words) (L.map fst cleanedTweets)

{- From the book:
wordFrequencyByLanguage' allLanguages cleanedTweets =
  (HM.fromList . L.map (\language ->
       (language, (frequency . concatMap words . L.map fst)
                    (L.filter (\tweet -> language == (snd tweet)) cleanedTweets)))) allLanguages

-- I cleaned up this definition to avoid multiple traversals of cleanedTweets
-- Each cleaned tweet should produce one (language, word frequency map) pair
-- Then all of those maps are combined.
-}

wordFrequencyByLanguage :: (Eq k, Hashable k,
                            Eq v, Hashable v,
                            Integral i) =>
                           [(k,[v])] -> HashMap k (HashMap v i)
wordFrequencyByLanguage = fromListWith (HM.unionWith (+))
                        . L.map (\(k, vs)-> (k, frequency vs))

probLanguageGivenWord ::
  ( Eq k, Hashable k,
    Eq v, Hashable v,
    Fractional f, Integral i
  ) => HashMap v i
    -> HashMap k i
    -> HashMap v (HashMap k i)
    -> v -> k -> [f]

probLanguageGivenWord
    languageFrequency
    wordFrequency
    wordFrequencyByLanguage
    language
    word
    =
    [pLanguage * pWordGivenLanguage / pWord, pLanguage, pWordGivenLanguage, pWord]
  where
    countTweets   = fromIntegral . sum $ elems languageFrequency
    countLanguage = fromIntegral $ lookupDefault 0 language languageFrequency

    countAllWords = fromIntegral . sum $ elems wordFrequency
    countWordsUsedInLanguage =  fromIntegral . sum . elems $
                                wordFrequencyByLanguage ! language
    countWord     = fromIntegral $ lookupDefault 0 word wordFrequency
    countWordInLanguage = fromIntegral $ lookupDefault 0 word
                              (wordFrequencyByLanguage ! language)
    pLanguage           = countLanguage / countTweets
    pWordGivenLanguage  = countWordInLanguage / countWordsUsedInLanguage
    pWord               = countWord / countAllWords

type Counts a = HashMap a Integer
type Lang = String
test129' :: IO ( Counts MyWord
               , Counts Lang
               , HashMap Lang (Counts MyWord))
test129' = do
  tweets <- test125
  let freqTable          = frequency tweets
  let uniqueTweets       = HM.keys freqTable
  let cleanedTweets      = zip (L.map snd uniqueTweets) (L.map (clean.fst) uniqueTweets)
  let languageFrequency  = (frequency . L.map fst) cleanedTweets
  let wordFreqByLang     = wordFrequencyByLanguage cleanedTweets
  let wordFrequency      = (frequency . concat) (L.map snd cleanedTweets)
  return (wordFrequency, languageFrequency, wordFreqByLang)

test129 :: IO (Lang -> MyWord -> [Double])
test129 = do
  (wordFrequency, languageFrequency, wordFrequencyByLanguage) <- test129'
  return $ probLanguageGivenWord languageFrequency wordFrequency wordFrequencyByLanguage

-- 17297 tweets
-- 239359 words
-- 47558 = sum . (L.map (sum.elems) . elems) $ wFBL

{-
Sanity checks:
(wF, lF, wFBL) <- test129'
sum . elems $ lF
sum . elems $ wF
sum . (L.map (sum.elems) . elems) $ wFBL


f <- test129
f "en" "casa"
f "es" "casa"
f "pt" "casa"

-}

-- probLanguage :: String -> HashMap String Integer -> Double
probLanguage :: (Eq k, Hashable k, Fractional f, Integral i) =>
  HashMap k i -> k -> f
probLanguage languageFrequency language = countLanguage / countTweets
  where
    countTweets   = fromIntegral . sum $ elems languageFrequency
    countLanguage = fromIntegral $ lookupDefault 0 language languageFrequency

-- probWordGivenLanguage :: String -> String ->HashMap String (HashMap String Integer) -> Double
probWordGivenLanguage ::
  ( Eq k, Hashable k
  , Eq v, Hashable v
  , Fractional f, Integral i
  ) => HashMap k (HashMap v i) -> k -> v -> f
probWordGivenLanguage wordFrequencyByLanguage language word =
    countWordInLanguage / countWordsUsedInLanguage
  where
    langFreq = wordFrequencyByLanguage ! language
    countWordInLanguage      = fromIntegral $ lookupDefault 0 word $ langFreq
    countWordsUsedInLanguage = fromIntegral . sum . elems          $ langFreq

-- probLanguageGivenMessage ::
--       String -> String
--    -> HashMap String Integer
--    -> HashMap String (HashMap String Integer)
--    -> Double
probLanguageGivenMessage ::
  ( Eq k, Hashable k
  , Fractional f
  , Integral i) =>
  HashMap k i -> HashMap k (HashMap MyWord i) -> MyWord -> k -> f
probLanguageGivenMessage languageFrequency wordFrequencyByLanguage message language =
    probLanguage languageFrequency language *
    product (L.map (probWordGivenLanguage wordFrequencyByLanguage language) (words message))

{-
languageClassifierGivenMessage ::
      Counts Lang
   -> HashMap Lang (Counts MyWord)
   -> String
   -> [(Lang, Double)]
-}
languageClassifierGivenMessage ::
  ( Eq k, Hashable k
  , Fractional f
  , Integral i) =>
  HashMap k i -> HashMap k (HashMap MyWord i) -> MyWord -> [(k, f)]
languageClassifierGivenMessage languageFrequency wordFrequencyByLanguage message =
    L.map (\lang-> (lang, probLangGM lang)) (keys languageFrequency)
  where probLangGM = probLanguageGivenMessage languageFrequency wordFrequencyByLanguage message

-- maxClassifier :: [(String, Double)] -> (String, Double)
maxClassifier :: Ord a => [(k, a)] -> (k, a)
maxClassifier = L.maximumBy (comparing snd)

test133 :: IO (MyWord -> (Lang, Double))
test133 = do
  (_wordFreq, languageFreq, wordFreqByLang) <- test129'
  return (maxClassifier . languageClassifierGivenMessage languageFreq wordFreqByLang)

phrases = [ "hi how are you doing today"
          , "it is a beautiful day outside"
          , "would you like to join me for lunch"
          , "vaya ud derecho"
          , "eres muy amable"
          , "le gusta a usted aquí"
          , "feliz cumpleaños"
          , "cest une bonne idée"
          , "il est très beau"
          ]

test134 :: IO [((Lang, Double), MyWord)]
test134 = do
  f <- test133
  return $ L.map (\ph -> (f ph, ph)) phrases
