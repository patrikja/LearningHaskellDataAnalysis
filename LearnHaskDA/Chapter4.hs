{-# LANGUAGE FlexibleContexts #-}
module LearnHaskDA.Chapter4 where
import Data.List
import Data.Convertible.Base
import Database.HDBC.Sqlite3
import Database.HDBC
import Graphics.EasyPlot
import LearnHaskDA.Chapter2

readIntColumn    :: [[SqlValue]] -> Int -> [Integer]
readDoubleColumn :: [[SqlValue]] -> Int -> [Double]
readStringColumn :: [[SqlValue]] -> Int -> [String]
readIntColumn    = readTypeColumn
readDoubleColumn = readTypeColumn
readStringColumn = readTypeColumn

readTypeColumn :: Convertible SqlValue b => [[SqlValue]] -> Int -> [b]
readTypeColumn sqlResult index = map (fromSql . (!!index)) sqlResult

queryDatabase :: FilePath -> String -> IO [[SqlValue]]
queryDatabase databaseFile sqlQuery = do
  conn <- connectSqlite3 databaseFile
  disconnect conn
  quickQuery' conn sqlQuery []
