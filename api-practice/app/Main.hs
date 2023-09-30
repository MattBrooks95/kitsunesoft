module Main (main) where

import System.Exit (die)
import qualified Data.ByteString as BS
import Utils.LoadEnv (
    runParseEnv
    )

main :: IO ()
main = do
    envFileContents <- BS.readFile ".env"
    envParseResult <- case runParseEnv envFileContents of
            Left err -> do
                print err
                die "failed to read the .env file"
            Right env -> return env
    print envParseResult
    print "main done"
