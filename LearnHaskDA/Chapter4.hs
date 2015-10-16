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
    $ map (mapFst time2year)
    $ filterDate "2014-01-01" aapl

filterDate :: Ord d => d -> [(d, a)] -> [(d, a)]
filterDate d = filter ((d<=).fst)

-- Date conversion
time2year :: String -> Double
time2year t = fromIntegral (diffDays (parseDay t) startDay) / 365

startDay :: Day
startDay = fromGregorian 2014 01 01

parseDay :: String -> Day
parseDay = parseTimeOrError True defaultTimeLocale (iso8601DateFormat Nothing)

-- Page 65

percentChange :: Double -> Double -> Double
percentChange first value  = 100.0 * (value - first) / first

applyPercentChangeToData :: [(Double, Double)] -> [(Double, Double)]
applyPercentChangeToData dataset = map (mapBoth (\d->d-start) (percentChange first)) dataset
  where  (start, first)  = head dataset

mapBoth :: (a->b) -> (c->d) -> (a, c) -> (b, d)
mapBoth f g (a, c) = (f a, g c)

mapFst :: (a->b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

mapSnd :: (a->b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

----------------
{-
aapl <- pullStockClosingPrices "aapl.sql" "aapl"
let aaplLastYear = filterDate "2013-11-01" aapl
let aaplPercent = applyPercentChangeToData $ map (mapFst time2year) aaplLastYear
plot (PNG "aapl_oneyear_pc.png") $ Data2D [Title "AAPL - One Year, % Change", Style Lines] [] $ aaplPercent

googl <- pullStockClosingPrices "googl.sql" "googl"
let googlLastYear = filterDate "2013-11-01" googl
let googlPercent = applyPercentChangeToData $ map (mapFst time2year) googlLastYear
plot (PNG "googl_oneyear_pc.png") $ Data2D [Title "GOOGL - One Year, % Change", Style Lines] [] $ googlPercent

msft <- pullStockClosingPrices "msft.sql" "msft"
let msftLastYear = filterDate "2013-11-01" msft
let msftPercent = applyPercentChangeToData $ map (mapFst time2year) msftLastYear
plot (PNG "msft_oneyear_pc.png") $ Data2D [Title "MSFT - One Year, % Change", Style Lines] [] $ msftPercent

let threePlots = plot (PNG "aapl_googl_msft_pc.png")
               [ Data2D [Title "AAPL  - One Year, % Change", Style Lines, Color Red]   [] aaplPercent
               , Data2D [Title "GOOGL - One Year, % Change", Style Lines, Color Blue]  [] googlPercent
               , Data2D [Title "MSFT  - One Year, % Change", Style Lines, Color Green] [] msftPercent
               ]
-}

-- TODO: think about placing the moving average in time.  Currently
--   the date is from the start of the window which means that the
--   data ends window days before the underlying data. Often the
--   opposite is wanted: the date is from the end of the period and
--   the data is window days into the past.

-- movingAverage :: Int -> [Double] -> [Double]
movingAverage :: (Real r, Fractional f) => Int -> [(k, r)] -> [(k, f)]
movingAverage window pairs
  | window >= length pairs  = [ (fst (head pairs), average (map snd pairs)) ]
  | otherwise               = (fst (head pairs), average (take window (map snd pairs))) :
                              movingAverage window (tail pairs)

-- map (take window) . tails

plotLastYear db = do
  prices <- pullStockClosingPrices (db ++ ".sql") db
  let lastYear = filterDate "2013-11-01" prices
  let percent = applyPercentChangeToData $ map (mapFst time2year) lastYear
  plot (PNG (db ++"_oneyear_pc.png"))
    $ Data2D [Title (db ++ " - One Year, % Change"), Style Lines] []
    $ percent

plotWithMovingAverage db = do
  prices      <- pullStockClosingPrices (db ++ ".sql") db
  let lastYear = filterDate "2013-11-01" prices
  let percent  = applyPercentChangeToData $ map (mapFst time2year) lastYear
  let ma       = movingAverage 20 percent
  plot (PNG (db ++ "_20dayma.png"))
    $ [ Data2D [Title (db ++ " - One Year, % Change"), Style Lines, Color Red  ] [] percent
      , Data2D [Title (db ++ " 20-Day MA"),            Style Lines, Color Black] [] ma
      ]

----------------
pullLongLat :: String -> String -> IO [(Double, Double)]
pullLongLat databaseFile database = do
  sqlResult <- queryDatabase databaseFile ("SELECT longitude, latitude FROM " ++ database)
  return $ zip  (readDoubleColumn sqlResult 0)
                (readDoubleColumn sqlResult 1)

pullLongLatMag :: String -> String -> IO [(Double, Double, Double)]
pullLongLatMag databaseFile database = do
  sqlResult <- queryDatabase databaseFile ("SELECT longitude, latitude, mag FROM " ++ database)
  return $ zip3  (readDoubleColumn sqlResult 0)
                 (readDoubleColumn sqlResult 1)
                 (readDoubleColumn sqlResult 2)
{-
coords <- pullLongLat "earthquakes.sql" "oneMonth"
plot (PNG "earthquakes.png") [Data2D [Title "Earthquakes", Color Red, Style Dots] [] coords ]
-}
