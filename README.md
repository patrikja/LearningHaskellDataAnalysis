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
```


```haskell
convertCSVFileToSQL "aapl.csv" "aapl.sql" "aapl" ["date STRING", "open REAL", "high REAL", "low REAL", "close REAL", "volume REAL", "adjclose REAL"]
```
