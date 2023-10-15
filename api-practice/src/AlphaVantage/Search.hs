{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module AlphaVantage.Search (
    SearchResults(..)
    , SearchResult(..)
    ) where

import AlphaVantage.Const as AVC
import System.FilePath (
    (</>)
    )
import Control.Monad.Catch (
    MonadThrow
    )
--import qualified Network.HTTP.Simple as NS
import qualified Data.ByteString as BS


import Utils.Urls

import AlphaVantage.MarketTypes (
    MarketItem(..)
    )

import Data.Aeson (
    ToJSON
    , FromJSON
    , parseJSON, Value (..), (.:)
    )
import qualified Data.Attoparsec.ByteString as AP
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Aeson.Types (prependFailure, typeMismatch)

data SearchResult = SearchResult {
    searchSymbol :: T.Text
    , searchName :: T.Text
    , searchItemType :: MarketItem
    , searchRegion :: T.Text
    , searchMarketopen :: T.Text
    , searchMarketClose :: T.Text
    , searchTimezone :: T.Text
    , searchCurrency :: T.Text
    , searchMatchScore :: T.Text
    } deriving (Show)

instance FromJSON SearchResult where
    parseJSON (Object v) = SearchResult
        <$> v .: "1. symbol"
        <*> v .: "2. name"
        <*> v .: "3. type"
        <*> v .: "4. region"
        <*> v .: "5. marketOpen"
        <*> v .: "6. marketClose"
        <*> v .: "7. timezone"
        <*> v .: "8. currency"
        <*> v .: "9. matchScore"
    parseJSON invalid =
        prependFailure "parsing SearchResult failed"
            (typeMismatch "Object" invalid)

newtype SearchResults = SearchResults {
        bestMatches :: [SearchResult]
    } deriving (Show, Generic, FromJSON)

--searchBaseUrl :: BS.ByteString
--searchBaseUrl = AVC.protocol
--    </> AVC.domain
--    </> AVC.queryEndpoint
--    <> "?"
--    <> mkQueryParams [("function", AVC.symbolSearch)]
--
--tickerSearchRequest :: MonadThrow m => String -> m NS.Request
--tickerSearchRequest targetTicker apiKey =
--    NS.parseRequest $ searchBaseUrl <> "&" <> mkQueryParamPair (apiKeyQueryP, apiKey)
