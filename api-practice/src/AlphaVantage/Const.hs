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

keywordsQueryP :: String
keywordsQueryP = "keywords"

searchBestMatches :: String
searchBestMatches = "bestMatches"
