{-# LANGUAGE OverloadedStrings, DeriveAnyClass, DeriveGeneric #-}
module AlphaVantage.MarketTypes (
    MarketItem(..)
    ) where

import Data.Aeson
import Data.Aeson.Types (typeMismatch, prependFailure)

data MarketItem = Equity
    deriving (Show)
instance FromJSON MarketItem where
    parseJSON (String "Equity") = return Equity
    parseJSON invalid =
        prependFailure "failed to parse marketItem type"
            (typeMismatch "Equity" invalid)
