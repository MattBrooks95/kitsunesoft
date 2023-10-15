module AlphaVantage.Fetches (
    searchTicker
    ) where

import qualified AlphaVantage.Urls as AVU
import QueryEnv.QueryEnv (QueryEnv(..))
import qualified Network.URL as U
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Reader (ReaderT, ask)
import AlphaVantage.Search (SearchResults)
import Network.HTTP.Client (httpLbs, responseBody, parseRequest)
import Data.Aeson (eitherDecode)

--TODO MonadThrow is necessary here for parseRequest, but I'm not sure how to handle it
--if it threw something. Wondering if I can wrap it (parseRequest) to return a Maybe instead
searchTicker :: (MonadIO m, MonadThrow m) => String -> ReaderT QueryEnv m (Either String SearchResults)
searchTicker ticker = do
    QueryEnv { qeApiKey=apiKey, qeHttp=manager } <- ask
    let searchUrl = AVU.search ticker (show apiKey)
    liftIO $ print $ "searchUrl:" ++ U.exportURL searchUrl
    request <- parseRequest (U.exportURL searchUrl)
    response <- liftIO $ httpLbs request manager
    return $ eitherDecode (responseBody response)


