module Main (main) where

import qualified Data.ByteString as BS
import qualified Data.Map as M
import Utils.LoadEnv (
    runParseEnv
    )

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
    print "main done"
