module AlphaVantage.Parsing.Search (
    ) where

import Data.Aeson
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import AlphaVantage.Search (SearchResults)
import AlphaVantage.Const (searchBestMatches)

--parseSearchResult :: Object -> AP.Parser a
--parseSearchResult = ob .: searchBestMatches
