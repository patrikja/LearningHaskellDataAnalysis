module LearnHaskDA.Chapter2 where
import Data.List
import Data.Either
import Text.CSV
import Database.HDBC
import Database.HDBC.Sqlite3

-- Compute the average of a list of values
average :: (Real a, Fractional b) => [a] -> b
average xs = realToFrac (sum xs) / fromIntegral (length xs)

-- Note that the Num class does not support division
--   (/) :: Fractional a =>   a -> a -> a
-- and that the Real class has types convertible to Rational
--   toRational :: Real a =>  a -> Rational

getColumnInCSV :: CSV -> String -> Either String Int
getColumnInCSV []          _          = Left $ "Empty CSV file"
getColumnInCSV (csvHead:_) columnName =
  case lookupResponse of
    Nothing  -> Left $ "The column '"++columnName++"' does not exist in this CSV file's header."
    Just x   -> Right x
  where
    lookupResponse = findIndex (columnName ==) csvHead

applyToColumnInCSV :: ([String] -> b) -> String -> CSV -> Either String b
applyToColumnInCSV func column csv = do
    columnIndex <- getColumnInCSV csv column
    return $ func (elements columnIndex)
  where  nfieldsInFile = length $ head csv
         elements ci = map (!!ci) records
         records = tail $ -- drops the header line
                   filter (\record -> nfieldsInFile == length record) csv
  -- TODO: loosen requirement on number of fields per line to
  --   just require the columnIndex to be indexable.

readColumn :: [String] -> [Double]
readColumn = map read
-- Note: quite a few typos in Chapter 2.
--   both readColumn and applyToColumnInCSV have errors in the book.

-- Opens a CSV file and applies a function to a column
-- Returns Either Error Message or the function result
applyToColumnInCSVFile ::
  ([String] -> b) -> String -> FilePath -> IO (Either String b)
applyToColumnInCSVFile func column inFileName = do
  -- Open and read the CSV file
  input <- readFile inFileName
  let records = parseCSV inFileName input
  -- Check to make sure this is a good csv file
  return $ either handleCSVError
                  (applyToColumnInCSV func column)
                  records
    where
      handleCSVError err = Left $ "This does not appear to be a CSV file: " ++ show err
      -- Note: Typo in the book (again) on page 31.
      --   (I have re-arranged the argument order as well.)

-- Example calls:
{-
λ> applyToColumnInCSVFile (average . readColumn) "mag" "LearnHaskDA/all_week.csv"
Right 1.8222112676056361
λ> applyToColumnInCSVFile (maximum . readColumn) "mag" "LearnHaskDA/all_week.csv"
Right 6.6
λ> applyToColumnInCSVFile (minimum . readColumn) "mag" "LearnHaskDA/all_week.csv"
Right (-0.39)
λ> applyToColumnInCSVFile (median . readColumn) "mag" "LearnHaskDA/all_week.csv"
Right 1.44
-}

-- ================================================================


-- Converts a CSV expression into an SQL database
-- Returns "Successful" if successful,
-- error message otherwise.
convertCSVToSQL ::
  String -> FilePath -> [String] -> CSV -> IO ()
convertCSVToSQL tableName outFileName fields records =
  if nfieldsInFile == nfieldsInFields then do

    conn <- connectSqlite3 outFileName     -- Open a connection
    run conn createStatement []            -- Create a new table
    stmt <- prepare conn insertStatement   -- Load file into table
    executeMany stmt (tail (filter ((nfieldsInFile ==) . length) sqlRecords))
    commit conn                            -- Commit changes
    disconnect conn                        -- Close the connection
    putStrLn "Successful"
  else
    putStrLn "The number of input fields differ from the csv file."
  where
    nfieldsInFile    = length $ head records
    nfieldsInFields  = length fields
    createStatement = "CREATE TABLE " ++ tableName ++ " (" ++ (intercalate ", " fields) ++")"
    insertStatement = "INSERT INTO "  ++ tableName ++ " VALUES (" ++
                         (intercalate ", " (replicate nfieldsInFile "?"))
                         ++ ")"
    sqlRecords = map (map toSql) records

-- Convert a CSV file to an SQL database file
convertCSVFileToSQL :: FilePath -> FilePath -> String -> [String] -> IO ()
convertCSVFileToSQL inFileName outFileName tableName fields = do
    input <- readFile inFileName  -- Open and read the CSV file
    let records = parseCSV inFileName input
    either handleCSVError convertTool records
  where
    convertTool = convertCSVToSQL tableName outFileName fields
    handleCSVError err = putStrLn $ "This does not appear to be a CSV file: " ++ show err

testConvert :: IO ()
testConvert = convertCSVFileToSQL "LearnHaskDA/all_week.csv" "earthquakes.sql" "oneWeek" ["time TEXT", "latitude REAL", "longitude REAL", "depth REAL", "mag REAL", "magType TEXT", "nst INTEGER", "gap REAL", "dmin REAL", "rms REAL", "net REAL", "id TEXT", "updated TEXT", "place TEXT", "type TEXT"]

testQuery :: IO [Double]
testQuery = do
  conn <- connectSqlite3 "earthquakes.sql"
  magnitudes <- quickQuery' conn "SELECT mag FROM oneWeek" []
  let ms = map (fromSql . head) magnitudes
  return ms
