# LearningHaskellDataAnalysis
Some notes and code from working through the book "Learning Haskell Data Analysis".


## Chapter 1 installs

TBD

## Chapter 2 installs

```shell
sudo apt-get install sqlite3
sudo apt-get install libghc-hdbc-sqlite3-dev
cabal install HDBC sqlite HDBC-sqlite3
cabal configure
```

## Chapter 3

## Chapter 4

```shell
wget 'http://real-chart.finance.yahoo.com/table.csv?s=AAPL&d=10&e=10&f=2014&g=d&a=11&b=12&c=1980&ignore=.csv' -O aapl.csv
wget 'http://real-chart.finance.yahoo.com/table.csv?s=GOOGL&d=10&e=10&f=2014&g=d&a=11&b=12&c=1980&ignore=.csv' -O googl.csv
wget 'http://real-chart.finance.yahoo.com/table.csv?s=MSFT&d=10&e=10&f=2014&g=d&a=11&b=12&c=1980&ignore=.csv'  -O msft.csv
```


```haskell
let tableHead = ["date STRING", "open REAL", "high REAL", "low REAL", "close REAL", "volume REAL", "adjclose REAL"]
convertCSVFileToSQL "aapl.csv"  "aapl.sql"  "aapl"  tableHead
convertCSVFileToSQL "googl.csv" "googl.sql" "googl" tableHead
convertCSVFileToSQL "msft.csv"  "msft.sql"  "msft" tableHead
```

(Typo on page 70 "RDEAL" := "REAL".)

```shell
wget 'http://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.csv' -O all_month.csv
```

```haskell
convertCSVFileToSQL "all_month.csv" "earthquakes.sql" "oneMonth" ["time TEXT", "latitude REAL", "longitude REAL", "depth REAL", "mag REAL", "magType TEXT", "nst INTEGER", "gap REAL", "dmin REAL", "rms REAL", "net REAL", "id TEXT", "updated TEXT", "place TEXT", "type TEXT"]
```

(Typo on page 75: remove the line starting with earthquakeCoordinates.)

## Chapter 5

```shell
cabal install exact-combinatorics
wget http://www.retrosheet.org/gamelogs/gl2014.zip
unzip gl2014.zip
cut -d, -f 1,4,7,10,11 GL2014.TXT > winloss2014.csv
cabal install erf
```

```haskell
convertCSVFileToSQL "winloss2014.csv" "winloss.sql" "winloss" ["date TEXT", "awayteam TEXT", "hometeam TEXT", "awayscore INTEGER", "homescore INTEGER"]
runsAtHome <- queryDatabase "winloss.sql" "SELECT hometeam, SUM(homescore) FROM winloss GROUP BY hometeam ORDER BY hometeam"
runsAtHome
runsAway   <- queryDatabase "winloss.sql" "SELECT awayteam, SUM(awayscore) FROM winloss GROUP BY awayteam ORDER BY awayteam"
runsAway
let runsHomeAway = zip (readDoubleColumn runsAtHome 1) (readDoubleColumn runsAway 1)
plot (PNG "HomeScoreAwayScore.png") $ Data2D [Title "Runs at Home (x axis) and Runs Away (y axis)"] [] runsHomeAway
let runsHomeAwayDiff = map (\(a,b) -> a-b) runsHomeAway
plot (PNG "HomeScoreAwayScoreDiff.png") $ Data2D [Title "Difference in Runs at Home and Runs Away"] [] $ zip [1..] runsHomeAwayDiff
average runsHomeAwayDiff
```

## Chapter 6

## Chapter 7

https://apps.twitter.com/

https://www.fpcomplete.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json

## Chapter 8

```shell
sudo apt-get install liblapack-dev
cabal install hmatrix
```

## Appendix

Just some simple examples of using regular expressions for text matching.
