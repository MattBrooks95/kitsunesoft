{-# LANGUAGE OverloadedStrings #-}
module Utils.Urls (
    mkQueryParams
    , mkQueryParamPair
    ) where
import qualified Data.ByteString.Lazy as BS

mkQueryParamPair :: (BS.ByteString, BS.ByteString) -> BS.ByteString
mkQueryParamPair (s1, s2) = s1 <> "=" <> s2

mkQueryParams :: [(BS.ByteString, BS.ByteString)] -> BS.ByteString
mkQueryParams pairs =
    BS.intercalate "&" $ map (\(s1, s2) -> s1 <> "=" <> s2) pairs
