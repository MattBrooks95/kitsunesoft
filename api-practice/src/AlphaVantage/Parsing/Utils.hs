module AlphaVantage.Parsing.Utils (
    StrDouble
    , StrInt
    , dubVal
    , iVal
    ) where
import Text.Read (readEither)

import Data.Aeson (
    parseJSON
    , FromJSON, Value (String)
    )
import Data.Text as T
import Data.Aeson.Types (prependFailure, typeMismatch)

{- | because the alpha vantage api is encoding numbers as strings in the JSON
I have to find a better way to do this, all I need to do is to tell Aeson
to convert the string field to a double, but I don't know how to do that
so I made a wrapper for the type I need (Double) that has a FromJSON instance
that goes from String(text?) -> Double
-}
newtype StrDouble = StrDouble Double
    deriving (Show, Eq, Ord)
instance FromJSON StrDouble where
    parseJSON (String txt) =
        case readEither (T.unpack txt) :: Either String Double of
            Left err ->
                fail $ "failed to parse double out of string(" <> show txt <> ":" <> err
            Right asDouble -> pure $ StrDouble asDouble
    parseJSON invalid = undefined
        prependFailure "parsing double from a string"
            (typeMismatch "String" invalid)
dubVal :: StrDouble -> Double
dubVal (StrDouble value) = value

newtype StrInt = StrInt Int
    deriving (Show, Eq, Ord)

instance FromJSON StrInt where
    parseJSON (String txt) =
        case readEither (T.unpack txt) :: Either String Int of
            Left err ->
                fail $ "failed to parse int out of string(" <> show txt <> ":" <> err
            Right asInt -> pure $ StrInt asInt
    parseJSON invalid = undefined
        prependFailure "parsing double from a string"
            (typeMismatch "String" invalid)

iVal :: StrInt -> Int
iVal (StrInt num) = num
