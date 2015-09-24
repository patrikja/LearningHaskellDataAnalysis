module LearnHaskDA.Median where
import System.Environment (getArgs)
import LearnHaskDA.Chapter1
main :: IO ()
main = do
  values <- getArgs
  print $ median $ map read values
