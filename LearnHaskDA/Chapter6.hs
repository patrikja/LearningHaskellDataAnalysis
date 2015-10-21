module LearnHaskDA.Chapter6 where
import Data.List
import Graphics.EasyPlot
import LearnHaskDA.Chapter2
import LearnHaskDA.Chapter4
import LearnHaskDA.Chapter5

--
covariance :: (Fractional a, Real a, Fractional b) => [a] -> [a] -> b
covariance xs ys = average (zipWith (\x y -> (x - xavg) * (y - yavg)) xs ys)
  where  xavg = average xs
         yavg = average ys

-- | Pearson r Correlation Coefficient
pearsonR :: (Floating a, Real a) => [a] -> [a] -> a
pearsonR xs ys = r
  where  xstdev = standardDeviation xs
         ystdev = standardDeviation ys
         r = covariance xs ys / (xstdev * ystdev)

-- | Pearson r-squared
pearsonRsqrd :: (Floating a, Real a) => [a] -> [a] -> a
pearsonRsqrd xs ys = pearsonR xs ys ^ 2

{-

homeRecord <- queryDatabase "winloss.sql" "SELECT homeTeam, SUM(homescore > awayscore), SUM(homescore), COUNT(*) FROM winloss GROUP BY homeTeam;"
awayRecord  <- queryDatabase "winloss.sql" "SELECT awayTeam, SUM(awayscore > homescore), SUM(awayscore), COUNT(*) FROM winloss GROUP BY awayTeam;"
let totalWins = zipWith (+) (readDoubleColumn homeRecord 1) (readDoubleColumn awayRecord 1)
let totalRuns = zipWith (+) (readDoubleColumn homeRecord 2) (readDoubleColumn awayRecord 2)
let totalGames = zipWith (+) (readDoubleColumn homeRecord 3) (readDoubleColumn awayRecord 3)
let winPercentage = zipWith (/) totalWins totalGames
let runsPerGame = zipWith (/) totalRuns totalGames
any (\x -> abs( (x - average runsPerGame) / standardDeviation runsPerGame) > 3) runsPerGame
any (\x -> abs( (x - average winPercentage) / standardDeviation winPercentage) > 3) winPercentage

plot (PNG "runs_and_wins.png") $ Data2D [Title "Runs Per Game VS Win % in 2014"] [] $ zip runsPerGame winPercentage

-}
-- | Compute the best-fit line (y = intercept + gradient * x)
linearRegression :: (Floating t, Real t) => [t] -> [t] -> (t, t)
linearRegression xs ys = (intercept, gradient)
  where  xavg      = average xs
         yavg      = average ys
         xstdev    = standardDeviation xs
         gradient  = covariance xs ys / (xstdev ^ 2)
         intercept = yavg - gradient * xavg

runsAndWins = do
  homeRecord <- queryDatabase "winloss.sql" "SELECT homeTeam, SUM(homescore > awayscore), SUM(homescore), COUNT(*) FROM winloss GROUP BY homeTeam;"
  awayRecord  <- queryDatabase "winloss.sql" "SELECT awayTeam, SUM(awayscore > homescore), SUM(awayscore), COUNT(*) FROM winloss GROUP BY awayTeam;"
  let totalWins     = zipWith (+) (readDoubleColumn homeRecord 1) (readDoubleColumn awayRecord 1)
  let totalRuns     = zipWith (+) (readDoubleColumn homeRecord 2) (readDoubleColumn awayRecord 2)
  let totalGames    = zipWith (+) (readDoubleColumn homeRecord 3) (readDoubleColumn awayRecord 3)
  let winPercentage = zipWith (/) totalWins totalGames
  let runsPerGame   = zipWith (/) totalRuns totalGames
  return $ (runsPerGame, winPercentage)

test109 = do (runsPerGame, winPercentage) <- runsAndWins
             return $ linearRegression runsPerGame winPercentage

test110 = do
  (runsPerGame, winPercentage) <- runsAndWins
  let (intercept, gradient) = linearRegression runsPerGame winPercentage
  plot (PNG "runs_and_wins_with_regression.png")
       [ Data2D [Title "Runs Per Game VS Win % in 2014"] [] (zip runsPerGame winPercentage)
       , Function2D [Title "Regression Line", Style Lines, Color Blue] [Range 3.3 4.7]
                    (\x -> intercept + x*gradient)
       ]
