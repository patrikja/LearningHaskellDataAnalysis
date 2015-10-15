{-# LANGUAGE FlexibleContexts #-}
module LearnHaskDA.Chapter4 where
import Data.List
import Data.Time
import Data.Time.Clock
import Data.Time.Calendar
import Data.Convertible.Base
import Database.HDBC.Sqlite3
import Database.HDBC
import Graphics.EasyPlot
import LearnHaskDA.Chapter2

readUTCTimeColumn:: [[SqlValue]] -> Int -> [UTCTime]
readIntegerColumn:: [[SqlValue]] -> Int -> [Integer]
readDoubleColumn :: [[SqlValue]] -> Int -> [Double]
readStringColumn :: [[SqlValue]] -> Int -> [String]
readUTCTimeColumn= readTypeColumn
readIntegerColumn= readTypeColumn
readDoubleColumn = readTypeColumn
readStringColumn = readTypeColumn

readTypeColumn :: Convertible SqlValue b => [[SqlValue]] -> Int -> [b]
readTypeColumn sqlResult index = map (fromSql . (!!index)) sqlResult

queryDatabase :: FilePath -> String -> IO [[SqlValue]]
queryDatabase databaseFile sqlQuery = do
  conn <- connectSqlite3 databaseFile
  result <- quickQuery' conn sqlQuery []
  disconnect conn
  return result
{-
  -- The code in the book on page 60 is wrong:
  let result = quickQuery' conn sqlQuery []
  disconnect conn
  result

is the same as

  disconnect conn
  quickQuery' conn sqlQuery []

which clearly disconnects before failing to do the query.

-}

pullStockClosingPrices :: FilePath -> String -> IO [(String, Double)]
pullStockClosingPrices databaseFile database = do
  sqlResult <- queryDatabase  databaseFile
                              (  "SELECT date, adjclose"
                               ++ " FROM " ++ database
                               ++" ORDER BY date")
  return $ zip  (readStringColumn sqlResult 0)
                (readDoubleColumn sqlResult 1)

testPlot = do
  aapl <- pullStockClosingPrices "aapl.sql" "aapl"
  plot (PNG "aapl.png")
    $ Data2D [Title "AAPL", Style Lines] []
    $ map (\(t, v) -> (time2year t, v))
    $ filter (("2014-01-01"<=).fst) aapl

-- Date conversion
time2year :: String -> Double
time2year t = fromIntegral (diffDays (parseDay t) startDay) / 365

startDay :: Day
startDay = fromGregorian 2014 01 01

parseDay :: String -> Day
parseDay = parseTimeOrError True defaultTimeLocale (iso8601DateFormat Nothing)

-- Page 65
