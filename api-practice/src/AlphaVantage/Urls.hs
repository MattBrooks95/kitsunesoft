module AlphaVantage.Urls (
    search
    , daily
    ) where

import qualified Network.URL as U
import qualified AlphaVantage.Const as Const (
    symbolSearch
    , queryUrlPath
    , functionQueryP
    , apiKeyQueryP
    , keywordsQueryP
    , domain
    , symbolDaily
    , symbolQueryP
    )
import AlphaVantage.Const (apiKeyQueryP)

https :: U.Protocol
https = U.HTTP True

defaultProtocol :: U.Protocol
defaultProtocol = https

defaultHost :: U.Host
defaultHost = U.Host {
    U.protocol=defaultProtocol
    , U.host=Const.domain
    , U.port=Nothing
    }

search :: String -> String -> U.URL
search searchString apiKey = U.URL {
    U.url_type=U.Absolute defaultHost
    , U.url_path=Const.queryUrlPath
    , U.url_params=[
        (Const.functionQueryP, Const.symbolSearch)
        , (Const.keywordsQueryP, searchString)
        , (Const.apiKeyQueryP, apiKey)
        ]
    }

daily :: String -> String -> U.URL
daily ticker apiKey = U.URL {
    U.url_type=U.Absolute defaultHost
    , U.url_path=Const.queryUrlPath
    , U.url_params=[
        (Const.functionQueryP, Const.symbolDaily)
        , (Const.symbolQueryP, ticker)
        , (Const.apiKeyQueryP, apiKey)
        ]
    }
