module QueryEnv.QueryEnv (
    QueryEnv(..)
    ) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import Network.HTTP.Client (Manager)

data QueryEnv = QueryEnv {
    qeHttp :: Manager
    , qeApiKey :: BS.ByteString
    }
