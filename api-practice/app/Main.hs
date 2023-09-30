module Main (main) where

import qualified Data.ByteString as BS
import Utils.LoadEnv (parseEnv)

main :: IO ()
main = do
    envFileContents <- BS.readFile ".env"
    env <- parseEnv
