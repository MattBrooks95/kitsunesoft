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

getEnvFromFile :: BS.ByteString -> IO (M.Map BS.ByteString BS.ByteString)
getEnvFromFile parseTarget = do
    case runParseEnv parseTarget of
        Left err -> do
            print $ "failed to parse env file:" ++ err
            return M.empty
        Right envParseResult -> return envParseResult

main :: IO ()
main = do
    envFileContents <- BS.readFile ".env"
    envParseResult <- getEnvFromFile envFileContents
    print $ "env map size:" <> (show . M.size) envParseResult
    let apiKeyLookup = M.lookup "API_KEY" envParseResult
    when (isNothing apiKeyLookup) (die "couldn't find api key")
    let apiKey = fromJust apiKeyLookup
    httpsManager <- newManager tlsManagerSettings
    print "main done"
    let testTicker = "toyota"
    let searchUrl = AVU.search testTicker (show apiKey)
    print $ "searchUrl:" ++ U.exportURL searchUrl
    request <- parseRequest (U.exportURL searchUrl)
    response <- httpLbs request httpsManager
    putStrLn $ "status code:" ++ show (statusCode $ responseStatus response)
    let responseContents = responseBody response
    print responseContents
    parseResult <-
            case eitherDecode responseContents :: Either String SearchResults of
                Left err -> die err
                Right asObject -> do
                    print "object decode successful"
                    print asObject
                    return asObject
    print $ (length . bestMatches) parseResult
    print $ intercalate "|" (map searchSymbol (bestMatches parseResult))
    --tickerSearchResults <- httpJSONEither (AVU.search testTicker)
    --print "done"
    --case tickerSearchResults of
    --    Left errMsg -> print errMsg
    --    Right results -> do
    --        print $ "request succeeded, results:" <> show results
