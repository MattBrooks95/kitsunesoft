{-# LANGUAGE OverloadedStrings #-}
module AlphaVantage.Search (
    --tickerSearch
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
