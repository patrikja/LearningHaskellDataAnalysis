module LearnHaskDA.Chapter1 where
import Data.List

median :: [Double] -> Double
median [] = 0
median xs | oddInLength = midVal
          | otherwise   = (beforeMidVal + midVal) / 2
  where  sortedList    = sort xs
         (middle, rem) = divMod (genericLength xs) 2
         oddInLength   = 1 == rem
         midVal        = genericIndex sortedList  middle
         beforeMidVal  = genericIndex sortedList (middle - 1)


vowels, vowelsEn, vowelsSv :: String
vowels   = vowelsSv
vowelsEn = "aeiouAEIOU"
vowelsSv = "aouåeiyäöAOUÅEIYÄÖ"


vowelIndices :: String -> [Integer]
vowelIndices = map fst
             . filter (\(_, letter) -> elem letter vowels)
             . zip [1..]
