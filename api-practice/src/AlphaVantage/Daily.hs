{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module AlphaVantage.Daily (
    DailyResult
    ) where

import qualified Data.Text as T
import Data.Aeson (
    FromJSON
    , parseJSON
    , Value(..)
    , (.:)
    )
import Data.Aeson.Types (
     prependFailure
    , typeMismatch
    )


import AlphaVantage.Types (OutputSize)
import GHC.Generics (Generic)

data DailyResult = DailyResult {
    dailyMetaData :: MetaData
    , dailyTimeSeries :: [TimeSeries]
    } deriving (Generic, Show)

instance FromJSON DailyResult where

data MetaData = MetaData {
    mdInformation :: T.Text
    , mdSymbol :: T.Text
    , mdLastRefreshed :: T.Text
    , mdOutputSize :: OutputSize
    , mdTimeZone :: T.Text
    } deriving (Show)

instance FromJSON MetaData where
    parseJSON (Object v) = MetaData
        <$> v .: "1. Information"
        <*> v .: "2. Symbol"
        <*> v .: "3. Last Refreshed"
        <*> v .: "4. Output Size"
        <*> v .: "5. Time Zone"
    parseJSON invalid =
        prependFailure "parsing MetaData failed"
            (typeMismatch "Object" invalid)

{- | TODO if the order of these keys changes, the FromJSON instance isn't going
to work anymore. The instance isn't assigning to the fields by name,
so if the fields order and the instances order are out of sync,
data will get assigned to the wrong attribute. You need to learn how
to actually use the Record's setter fields
-}
data TimeSeries = TimeSeries {
    tsOpen :: Double
    , tsHigh :: Double
    , tsLow :: Double
    , tsClose :: Double
    , tsVolume :: Int
    } deriving Show

instance FromJSON TimeSeries where
    parseJSON (Object v) = TimeSeries
        -- is this going to turn these into type `Double` for me???
        <$> v .: "1. open"
        <*> v .: "2. high"
        <*> v .: "3. low"
        <*> v .: "4. close"
        <*> v .: "5. volume"
    parseJSON invalid =
        prependFailure "parsing TimeSeries failed"
            (typeMismatch "Object" invalid)
