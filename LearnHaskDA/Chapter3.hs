module LearnHaskDA.Chapter3 (module LearnHaskDA.Chapter3) where
import Text.CSV
-- import Data.List
import Text.Regex.Posix ((=~))
import System.Environment (getArgs)
import Control.Monad

eitherPoor = parseCSVFromFile "LearnHaskDA/poorFieldCounts.csv"

countFieldsInEachRecord :: CSV -> [Int]
countFieldsInEachRecord csv = map length (init csv)

lineNumbersWithIncorrectCount :: CSV -> [(Integer, Int)]
lineNumbersWithIncorrectCount (fields:csv) = filter (\(_, thisCount) -> thisCount /= nfields) lineNoCountPairs
  where  nfields  = length fields
         count    = countFieldsInEachRecord csv
         lineNoCountPairs = zip [1..] count



-- wget -O huckfinn.txt http://www.gutenberg.org/ebooks/76.txt.utf-8
-- cabal install regex-posix

myGrep :: String -> FilePath -> IO ()
myGrep myRegex filename = do
  fileSlurp <- readFile filename
  mapM_ putStrLn $ filter (=~ myRegex) (lines fileSlurp)

main :: IO ()
main = do
  (myRegex:filenames) <- getArgs
  forM_ filenames (myGrep myRegex)
