{-# LANGUAGE OverloadedStrings #-}
module Utils.LoadEnv (
    parseEnv
    ) where

import qualified Data.Map as M
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.Word8 as W

parseEnv :: AP.Parser (M.Map BS.ByteString BS.ByteString)
parseEnv = M.fromList <$> AP.sepBy' parseLine (AP.word8 W._lf)

parseLine :: AP.Parser (BS.ByteString, BS.ByteString)
parseLine = do
    name <- parseWord
    _ <- AP.word8 W._equal
    val <- parseWord
    pure (name, val)

parseWord :: AP.Parser BS.ByteString
parseWord = AP.takeWhile (AP.inClass "a-zA-Z_1-9")
