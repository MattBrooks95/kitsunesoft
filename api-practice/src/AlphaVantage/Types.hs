{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module AlphaVantage.Types (
    DataType(..)
    , Function(..)
    , OutputSize(..)
    ) where

import GHC.Generics

import Data.Aeson (
    FromJSON
    )

data DataType = JSON | CSV
    deriving (Show)


data Function = SYMBOL_SEARCH
    deriving (Show)

{- | Compact is only the last 100 data points
Full is the full history (20+) years of data
-}
data OutputSize = Compact | Full
    deriving (Generic, FromJSON, Show)
