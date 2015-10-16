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
