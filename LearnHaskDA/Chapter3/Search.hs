module LearnHaskDA.Chapter3.Search where
import Text.CSV
import Data.List
import Text.Regex.Posix ((=~))
import LearnHaskDA.Chapter2

identifyMatchingFields :: (String -> Bool)
                       -> [String]
                       -> Int
                       -> [String]
                       -> [(String, String, String)]
identifyMatchingFields myStringCmpFunc headings idColumnIndex record =
    filter (\(_, _, field) -> myStringCmpFunc field) keyvalue
  where  nfields = length headings
         keyvalue = zip3 (replicate nfields (record !! idColumnIndex))
                         headings
                         record
testP50 :: [(String, String, String)]
testP50 = identifyMatchingFields (=~ "Journ")
                                 ["Id", "Name", "Profession", "Location"]
                                 0
                                 ["1", "Clark Kent", "Journalist", "Metropolis"]

----------------

identifyInCSV :: (String -> Bool) -> CSV -> String ->
                 Either String [(String, String, String)]
identifyInCSV myFieldFunc csv idColumn =
    fmap (\ci -> concatMap (identifyMatchingFields myFieldFunc (head csv) ci)
                           (tail csv))
         columnIndex
  where headings    = head csv
        columnIndex = getColumnInCSV csv idColumn

testP51 = fmap (either (\error -> Left "CSV Problem")
                       (\csv -> identifyInCSV (=~ "PA") csv "Number"))
               (parseCSVFromFile "poordata.csv")

----------------

identifyInCSVFile :: (String -> Bool) -> String -> String ->
                     IO (Either String [(String, String, String)])
identifyInCSVFile myStringCmpFunc inFileName idColumn = do
  fmap (either (\err -> Left "This does not appear to be a CSV file")
               (\csv -> identifyInCSV myStringCmpFunc (init csv) idColumn))
     $ parseCSVFromFile inFileName

testP52 = identifyInCSVFile (=~ "^$") "poordata.csv" "Number"
testP53 = identifyInCSVFile (=~ "^\\s*$") "poordata.csv" "Number"

----------------
identifyInCSVFileFromColumn ::
  (String -> Bool) -> String -> String -> String ->
  IO (Either String [(String, String, String)])
identifyInCSVFileFromColumn myRegexFunc inFileName idColumn desiredHeading = do
  mapIO (mapEither (filter (\(_, heading, _) -> heading == desiredHeading)))
     $ identifyInCSVFile myRegexFunc inFileName idColumn

mapIO = fmap
mapEither = fmap

regexUSdate = "^[1-9][0-9]?/[1-9][0-9]?/[12][0-9][0-9][0-9]$"
testP54 = identifyInCSVFileFromColumn (not . (=~ regexUSdate)) "poordata.csv" "Number" "Birthday"
