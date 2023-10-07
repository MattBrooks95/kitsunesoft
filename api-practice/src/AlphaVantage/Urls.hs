module AlphaVantage.Urls (
    search
    ) where

import qualified Network.URL as U
import qualified AlphaVantage.Const as Const (
    symbolSearch
    , queryUrlPath
    , functionQueryP
    , apiKeyQueryP
    , keywordsQueryP
    , domain
    )
import AlphaVantage.Const (apiKeyQueryP)

https :: U.Protocol
https = U.HTTP True

defaultProtocol :: U.Protocol
defaultProtocol = https

search :: String -> String -> U.URL
search searchString apiKey = U.URL {
    U.url_type=U.Absolute $ U.Host {
        U.protocol=defaultProtocol
        , U.host=Const.domain
        , U.port=Nothing
        }
    , U.url_path=Const.queryUrlPath
    , U.url_params=[
        (Const.functionQueryP, Const.symbolSearch)
        , (Const.keywordsQueryP, searchString)
        , (Const.apiKeyQueryP, apiKey)
        ]
    }
