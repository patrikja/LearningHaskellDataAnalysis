{-# LANGUAGE FlexibleContexts #-}
module LearnHaskDA.Chapter8 where
import Numeric.LinearAlgebra as LA
import Data.List as L
import Data.HashMap.Strict as HM
import qualified LearnHaskDA.Chapter4 as Ch4
import qualified LearnHaskDA.Chapter6 as Ch6
import qualified LearnHaskDA.Chapter7 as Ch7 (Counts, MyWord, clean, frequency, wordFrequencyByFst)
import Test.QuickCheck

test140 = do
    tweetsEnglish <- Ch4.queryDatabase "tweets.sql" "SELECT user, message FROM tweets WHERE language='en'"
    let tweets = Ch4.readTwoColumns 0 1 tweetsEnglish :: [(String, String)]
    let freqTable = Ch7.frequency tweets
    print $ HM.size freqTable
    let uniqueTweets = keys freqTable
    let cleanedTweets = L.map (\(user, mess) -> (user, Ch7.clean mess)) uniqueTweets
    let wordFrequency = Ch7.frequency $ concatMap snd cleanedTweets
    return (cleanedTweets, wordFrequency)

test141 = do
    (cleanedTweets, wordFrequency) <- test140
    let topWords = take 25 $ sortBy (\(_, c1) (_, c2) -> compare c2 c1) $ HM.toList wordFrequency
    return topWords

test142 = do
    (cleanedTweets, wordFrequency) <- test140
    let notStopWords = L.filter (\(word, _) -> notElem word stopWords) (HM.toList wordFrequency)
    let topWords = take 25 . L.map fst $ sortBy (\(_, c1) (_, c2) -> compare c2 c1) notStopWords
    return topWords


stopWords :: [String]
stopWords = ["", "a", "about", "above", "above", "across", "after",
  "afterwards", "again", "against", "all", "almost", "alone", "along",
  "already", "also", "although", "always", "am", "among", "amongst",
  "amoungst", "amount", "an", "and", "another", "any", "anyhow",
  "anyone", "anything", "anyway", "anywhere", "are", "around", "as",
  "at", "back", "be", "became", "because", "become", "becomes",
  "becoming", "been", "before", "beforehand", "behind", "being",
  "below", "beside", "besides", "between", "beyond", "bill", "both",
  "bottom", "but", "by", "call", "can", "cannot", "cant", "co", "con",
  "could", "couldnt", "cry", "de", "describe", "detail", "do", "done",
  "dont", "down", "due", "during", "each", "eg", "eight", "either",
  "eleven", "else", "elsewhere", "empty", "enough", "etc", "even",
  "ever", "every", "everyone", "everything", "everywhere", "except",
  "few", "fifteen", "fifty", "fill", "find", "fire", "first", "five",
  "for", "former", "formerly", "forty", "found", "four", "from",
  "front", "full", "further", "get", "give", "go", "got", "had", "has",
  "hasnt", "have", "he", "hence", "her", "here", "hereafter", "hereby",
  "herein", "hereupon", "hers", "herself", "him", "himself", "his",
  "how", "however", "https", "hundred", "i", "ie", "if", "im", "in", "inc",
  "indeed", "interest", "into", "is", "it", "its", "itself", "just",
  "keep", "last", "latter", "latterly", "least", "less", "ltd", "made",
  "many", "may", "me", "meanwhile", "might", "mill", "mine", "more",
  "moreover", "most", "mostly", "move", "much", "must", "my", "myself",
  "name", "namely", "neither", "need", "never", "nevertheless",
  "next", "nine", "no", "nobody", "none", "noone", "nor", "not",
  "nothing", "now", "nowhere", "of", "off", "often", "on", "once",
  "one", "only", "onto", "or", "other", "others", "otherwise", "our",
  "ours", "ourselves", "out", "over", "own", "part", "per", "perhaps",
  "please", "put", "rather", "re", "same", "see", "seem", "seemed",
  "seeming", "seems", "serious", "several", "she", "should", "show",
  "side", "since", "sincere", "six", "sixty", "so", "some", "somehow",
  "someone", "something", "sometime", "sometimes", "somewhere", "still",
  "such", "system", "take", "ten", "than", "that", "the", "their",
  "them", "themselves", "then", "thence", "there", "thereafter",
  "thereby", "therefore", "therein", "thereupon", "these", "they",
  "thick", "thin", "third", "this", "those", "though", "three",
  "through", "throughout", "thru", "thus", "to", "together", "too",
  "top", "toward", "towards", "twelve", "twenty", "two", "un", "under",
  "until", "up", "upon", "us", "very", "via", "want", "was", "we",
  "well", "were", "what", "whatever", "when", "whence", "whenever",
  "where", "whereafter", "whereas", "whereby", "wherein", "whereupon",
  "wherever", "whether", "which", "while", "whither", "who", "whoever",
  "whole", "whom", "whose", "why", "will", "with", "within", "without",
  "would", "yet", "you", "your", "youre", "yours", "yourself",
  "yourselves", "rt", "mt", "u"]

bookTopWords = ["like", "amp", "day", "good", "new", "love", "time", "follow", "great",
  "today", "make", "lot", "people", "video", "know", "life", "happy",
  "look", "think", "girl", "win", "photo", "way", "little", "really"]

myTopWords = ["amp","day","new","like","love","good","great","video",
              "make","win","follow","life","pill","today","happy","time",
              "chance","photo","posted","birthday","lot","brain","know",
              "limitless","think"]

topWords = bookTopWords

-- page 143
type UserName = String
type UserWordCount = [[Double]]
test143 :: IO (HashMap UserName (Ch7.Counts Ch7.MyWord), UserWordCount)
test143 = do
    (cleanedTweets, wordFrequency) <- test140
    let wordFrequencyByUser = Ch7.wordFrequencyByFst cleanedTweets
    let getFrequencyOfWordByUser user word = fromIntegral $ (HM.lookupDefault 0 word (wordFrequencyByUser HM.! user)) :: Double
    let allUsers = keys wordFrequencyByUser
    let wordMatrix = L.map (\user -> L.map (getFrequencyOfWordByUser user) topWords) allUsers
    return (wordFrequencyByUser, wordMatrix)

----------------

test150 = do
    homeRecord <- Ch4.queryDatabase "winloss.sql" "SELECT homeTeam, SUM(homescore > awayscore), SUM(homescore), COUNT(*) FROM winloss GROUP BY homeTeam;"
    awayRecord <- Ch4.queryDatabase "winloss.sql" "SELECT awayTeam, SUM(awayscore > homescore), SUM(awayscore), COUNT(*) FROM winloss GROUP BY awayTeam;"
    let totalWins     = zipWith (+) (Ch4.readDoubleColumn homeRecord 1) (Ch4.readDoubleColumn awayRecord 1)
    let totalRuns     = zipWith (+) (Ch4.readDoubleColumn homeRecord 2) (Ch4.readDoubleColumn awayRecord 2)
    let totalGames    = zipWith (+) (Ch4.readDoubleColumn homeRecord 3) (Ch4.readDoubleColumn awayRecord 3)
    let winPercentage = zipWith (/) totalWins totalGames
    let runsPerGame   = zipWith (/) totalRuns totalGames
    let baseball      = L.map (\(a,b) -> [a,b]) $ zip winPercentage runsPerGame
    let p@(baseballMean, baseballCovMatrix) = meanCov $ fromLists baseball
    return p

-- eigSH :: Field t => Herm t -> (Vector Double, Matrix t)
test151 = do
  (baseballMean, baseballCovMatrix) <- test150
  let p@(baseballEvalues, baseballEvectors) = eigSH (sym baseballCovMatrix)
  return p

-- page 154
-- principalComponentAnalysis :: [[Double]] -> Integer -> Matrix Double
principalComponentAnalysis records top = featureVectors <> topOrderedEvectors
-- Some unnecessary tr in the book:
--    tr $ mul (tr topOrderedEvectors) (tr featureVectors)
  where  featureVectors  = fromLists records
         (_, covMatrix)  = meanCov featureVectors
         (_, evectors)   = eigSH (trustSym covMatrix)
         topOrderedEvectors = takeColumns (fromInteger top) evectors

{-
-- Done: pull request for hmatrix changing the type of meanCov to use Herm
https://github.com/albertoruiz/hmatrix/pull/157
https://github.com/albertoruiz/hmatrix/blob/master/packages/base/src/Internal/Container.hs

meanCovH :: Matrix Double -> (Vector Double, Herm Double)
meanCovH x = (med, cov) where
    r    = rows x
    k    = 1 / fromIntegral r
    med  = konst k r <> x
    meds = konst 1 r `outer` med
    xc   = x - meds
    cov  = scale (recip (fromIntegral (r-1))) (mTm xc)
-}

-- let pcaMatrix = principalComponentAnalysis wordsMatrix 5

-- euclideanDistanceSqrdFromRow :: Matrix Double -> Int -> Vector Double
euclideanDistanceSqrdFromRow :: (Num t, Num (Vector t), Container Vector t) => Matrix t -> Int -> Vector t
euclideanDistanceSqrdFromRow records row = sumOfSquaresVector
  where
    d = cols records
    n = rows records
    theRow = subMatrix (row, 0) (1, d) records
    copyMatrix = repmat theRow n 1
    diffMatrix      = records    - copyMatrix  -- Pointwise operation
    diffSqrdMatrix  = diffMatrix * diffMatrix  -- Pointwise operation
    sumOfSquaresVector = sum $ toColumns diffSqrdMatrix

orderBookSimplified :: Eq a => [a] -> [a] -> [Int]
orderBookSimplified unsorted = concatMap (`L.elemIndices` unsorted)

test156' :: Ord a => [a] -> [Int]
test156' xs = orderBookSimplified xs (nub $ sort xs)

-- Much faster (better complexity):
test156 :: Ord a => [a] -> [Int]
test156 = L.map snd . sort . (`zip` [0..])

qc156 xs = test156 xs == test156' xs

findClosestFeaturesToRow :: Matrix Double -> Int -> Int -> [Int]
findClosestFeaturesToRow records row knn = take knn orderOfFeatures
  where  orderOfFeatures = test156 sumOfSquares
         sumOfSquares    = LA.toList $ euclideanDistanceSqrdFromRow records row

{-
test157 = do
  (_, wordMatrix) <- test143
  return
-}
