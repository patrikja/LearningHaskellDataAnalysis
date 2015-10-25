module LearnHaskDA.Chapter5 where
--import Math.Combinatorics.Exact.Binomial
import Numeric.SpecFunctions (choose)
import LearnHaskDA.Chapter2
import LearnHaskDA.Chapter4
import Graphics.EasyPlot
import System.Random
import Data.Number.Erf

{- Using Math.Combinatorics.Exact.Binomial in the exact-combinatorics
package we could have a more general type here and an exact, yet
efficient, computation of choose. But exact-combinatorics is not on
stackage so I changed to the approximation (from Numeric.SpecFunctions
which is on stackage). We only use it for plotting anyway.

-- probabilityMassFunction :: (Integral i, Num n) => i -> i -> n -> n
-}
probabilityMassFunction :: Int -> Int -> Double -> Double
probabilityMassFunction k n p = (n `choose` k)
                              * p^k * (1-p)^(n-k)
plot81 :: IO Bool
plot81 = plot (PNG "coinflips.png") $
              Function2D [Title "Coin Flip Probabilities"]
                         [Range 0 1000]
                         (\k -> probabilityMassFunction (floor k) 1000 0.5)

-- almost equal would be enough
sanitycheck :: Bool
sanitycheck = 1 == sum (map (\k -> probabilityMassFunction k 1000 0.5) [0..1000])

-- sumMid :: (Fractional a, Integral i) => i -> a
sumMid :: Int -> Double
sumMid d = sum $ map (\k -> probabilityMassFunction k 1000 0.5) [500-d .. 500+d]

----------------

standardDeviation :: (Floating a, Real a) => [a] -> a
standardDeviation values = sqrt (sum [(x-mu)^2 | x <- values]) / sqrt_nm1
  where  mu = average values
         sqrt_nm1 = sqrt $ fromIntegral (length values - 1)


select place = "SELECT "++place++"team, SUM("++place++"score) "++
                 "FROM winloss "++
                 "GROUP BY "++ place ++"team " ++
                 "ORDER BY "++ place ++"team"

test91 = do
  runsAtHome <- queryDatabase "winloss.sql" (select "home")
  runsAway   <- queryDatabase "winloss.sql" (select "away")
  let runsHomeAway = zip (readDoubleColumn runsAtHome 1) (readDoubleColumn runsAway 1)
  plot (PNG "HomeScoreAwayScore.png") $ Data2D [Title "Runs at Home (x axis) and Runs Away (y axis)"] [] runsHomeAway
  let runsHomeAwayDiff = map (\(a,b) -> a-b) runsHomeAway
  plot (PNG "HomeScoreAwayScoreDiff.png") $ Data2D [Title "Difference in Runs at Home and Runs Away"] [] $ zip [1..] runsHomeAwayDiff
  let n = fromIntegral (length runsHomeAwayDiff)
      avg = average runsHomeAwayDiff
      stdDev = standardDeviation runsHomeAwayDiff
      stdErr = stdDev / sqrt n
  return (avg, stdDev, stdErr)

--

from = invnormcdf ((1-95/100)/2 :: Double)
to = - from

test95 = test91 >>= \ (avg, stdDev, stdErr) ->
         return (avg + from*stdErr, avg + to*stdErr)

-- The 95% confidence interval is (-14.9, 26.6) which contains zero so
-- the null hypothesis could not be rejected.
