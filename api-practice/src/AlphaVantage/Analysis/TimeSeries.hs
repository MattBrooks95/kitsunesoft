module TimeSeries (
    highAverage
    , lowAverage
    ) where

import AlphaVantage.Daily (
    TimeSeries(..)
    )
import AlphaVantage.Parsing.Utils (dubVal)

averageField :: (TimeSeries -> Double) -> [TimeSeries] -> Double
averageField access tss =
    if numItems == 0
    then foldl (\acc ts -> acc + access ts) 0.0 tss / fromIntegral numItems
    else 0.0
    where
        numItems = length tss

highAverage :: [TimeSeries] -> Double
highAverage = averageField (dubVal . tsHigh)

lowAverage :: [TimeSeries] -> Double
lowAverage = averageField (dubVal . tsLow)
