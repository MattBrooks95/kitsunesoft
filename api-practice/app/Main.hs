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

-- helper that runs the IO action if the requisite Either is a Right
-- I feel like there's probably some helper in the standard lib for this
-- that is more general
-- this is nice for use in GHCi
runRight :: Either e a -> (a -> IO c) -> IO (Either e c)
runRight (Left e) _ = return $ Left e
runRight (Right use) act = Right <$> act use

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

-- up next, refactor the env variable getting and manager setup into a setup
-- function, so that they can be used in ghci

getEnv :: FilePath -> IO (M.Map BS.ByteString BS.ByteString)
getEnv fp = do
    envFileContents <- BS.readFile fp
    getEnvFromFile envFileContents

mkHttpManager_ :: ManagerSettings -> IO Manager
mkHttpManager_ = newManager

mkHttpManager :: IO Manager
mkHttpManager = mkHttpManager_ tlsManagerSettings

initialize :: IO (Either String QueryEnv)
initialize = do
    envMap <- getEnv ".env"
    let apiKeyLookup = M.lookup "API_KEY" envMap
    httpsManager <- mkHttpManager
    case apiKeyLookup of
        Nothing -> return $ Left "couldn't find api key in environment map"
        Just key ->
            return $ Right $ QueryEnv {
                apiKey=key
                , http=httpsManager
                }
        --Left "couldn't find api key in environment map")

main :: IO ()
main = do
    _queryEnvEither <- initialize
    -- is there a cleaner way to do this? I wanted to use "when"
    queryEnv@(QueryEnv { http=httpsManager, apiKey=key }) <- case _queryEnvEither of
            Left errMsg -> die errMsg
            Right qv -> return qv
    let testTicker = "toyota"
    let searchUrl = AVU.search testTicker (show key)
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
