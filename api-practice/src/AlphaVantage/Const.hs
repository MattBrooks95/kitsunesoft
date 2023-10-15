{-# LANGUAGE OverloadedStrings #-}
module AlphaVantage.Const (
    symbolSearch
    , protocol
    , domain
    , queryEndpoint
    , apiKeyQueryP
    , functionQueryP
    , queryUrlPath
    , keywordsQueryP
    , searchBestMatches
    , symbolDaily
    , symbolQueryP
    ) where

functionQueryP :: String
functionQueryP = "function"

protocol :: String
protocol = "https"

domain :: String
domain = "www.alphavantage.co"

queryEndpoint :: String
queryEndpoint = "query"

symbolSearch :: String
symbolSearch = "SYMBOL_SEARCH"

apiKeyQueryP :: String
apiKeyQueryP = "apikey"

queryUrlPath :: String
queryUrlPath = "query"

symbolDaily :: String
symbolDaily = "TIME_SERIES_DAILY"

keywordsQueryP :: String
keywordsQueryP = "keywords"

searchBestMatches :: String
searchBestMatches = "bestMatches"

symbolQueryP :: String
symbolQueryP = "symbol"
