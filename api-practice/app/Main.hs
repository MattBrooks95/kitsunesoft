{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import Utils.LoadEnv (
    runParseEnv
    )
import Data.Maybe (
    isNothing
    , fromJust
    )
import Control.Monad (
    when
    )
import Control.Monad.Reader

import System.Exit

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status (statusCode)
import qualified Network.URL as U

import qualified AlphaVantage.Urls as AVU
import Data.Aeson (decode, eitherDecode)
import AlphaVantage.Search (
    SearchResults (bestMatches)
    , SearchResult(..)
    )
import Data.Text (intercalate)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Catch (MonadThrow, MonadCatch (catch))

data QueryEnv = QueryEnv {
    http :: Manager
    , apiKey :: BS.ByteString
    }

getEnvFromFile :: BS.ByteString -> IO (M.Map BS.ByteString BS.ByteString)
getEnvFromFile parseTarget = do
    case runParseEnv parseTarget of
        Left err -> do
            print $ "failed to parse env file:" ++ err
            return M.empty
        Right envParseResult -> return envParseResult

--TODO MonadThrow is necessary here for parseRequest, but I'm not sure how to handle it
--if it threw something. Wondering if I can wrap it (parseRequest) to return a Maybe instead
searchTicker :: (MonadIO m, MonadThrow m) => String -> ReaderT QueryEnv m (Either String SearchResults)
searchTicker ticker = do
    QueryEnv { apiKey=apiKey, http=manager } <- ask
    let searchUrl = AVU.search ticker (show apiKey)
    liftIO $ print $ "searchUrl:" ++ U.exportURL searchUrl
    request <- parseRequest (U.exportURL searchUrl)
    response <- liftIO $ httpLbs request manager
    return $ eitherDecode (responseBody response)

main :: IO ()
main = do
    envFileContents <- BS.readFile ".env"
    envParseResult <- getEnvFromFile envFileContents
    print $ "env map size:" <> (show . M.size) envParseResult
    let apiKeyLookup = M.lookup "API_KEY" envParseResult
    when (isNothing apiKeyLookup) (die "couldn't find api key")
    let apiKey = fromJust apiKeyLookup
    httpsManager <- newManager tlsManagerSettings
    let queryEnv = QueryEnv {
        apiKey=apiKey
        , http=httpsManager
        }
    print "main done"
    let testTicker = "toyota"
    let searchUrl = AVU.search testTicker (show apiKey)
    print $ "searchUrl:" ++ U.exportURL searchUrl
    request <- parseRequest (U.exportURL searchUrl)
    response <- httpLbs request httpsManager
    putStrLn $ "status code:" ++ show (statusCode $ responseStatus response)
    let responseContents = responseBody response
    print responseContents
    tryToRequest <- runReaderT (searchTicker testTicker) queryEnv
    parseResult <-
            case tryToRequest of
                Left err -> die err
                Right asObject -> do
                    print "object decode successful"
                    print asObject
                    return asObject
    print parseResult
            --case eitherDecode responseContents :: Either String SearchResults of
            --    Left err -> die err
            --    Right asObject -> do
            --        print "object decode successful"
            --        print asObject
            --        return asObject
    --print $ (length . bestMatches) parseResult
    --print $ intercalate "|" (map searchSymbol (bestMatches parseResult))

    --tickerSearchResults <- httpJSONEither (AVU.search testTicker)
    --print "done"
    --case tickerSearchResults of
    --    Left errMsg -> print errMsg
    --    Right results -> do
    --        print $ "request succeeded, results:" <> show results
