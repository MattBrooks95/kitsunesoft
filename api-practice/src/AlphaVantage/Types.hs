module AlphaVantage.Types (
    DataType(..)
    , Function(..)
    ) where

data DataType = JSON | CSV
    deriving (Show)


data Function = SYMBOL_SEARCH
    deriving (Show)
