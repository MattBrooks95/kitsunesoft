{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module DataConfig.DataConfig (
    DataConfig(..)
    , getDataConfig
    , DailyConfig(..)
    ) where

import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as BS

data DailyConfig = DailyConfig {
    tickers :: [String]
    } deriving (Generic, FromJSON, Show)

data DataConfig = DataConfig {
    dcDaily :: DailyConfig
    } deriving (Generic, FromJSON, Show)

getDataConfig :: FilePath -> IO (Either String DataConfig)
getDataConfig fp = do
    fileContents <- BS.readFile fp
    return $ case eitherDecode fileContents :: Either String DataConfig of
        Left err -> Left $ "failed to parse data config at " <> fp <> " with error" <> err
        Right dc -> Right dc
