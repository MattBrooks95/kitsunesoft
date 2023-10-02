{-# LANGUAGE QuasiQuotes,OverloadedStrings #-}
module Test.Utils.LoadEnv (
    testParse,
    ) where

import Test.HUnit
import Data.Map as M
import Text.RawString.QQ
import Data.Attoparsec.ByteString as AP
import Data.Either (
    isLeft
    )

import Utils.LoadEnv (
    parseEnv
    )

testParse :: Test
testParse = TestList [
    TestCase (
        assertEqual "parses two env vars"
        (Right $ M.fromList [("var1", "val1"), ("var2", "val2")])
        (AP.parseOnly parseEnv "var1=val1\nvar2=val2")
    )
    , TestCase (
        assertEqual "parses one env var"
        (Right $ M.fromList [("var1", "val1"), ("var2", "val2")])
        (AP.parseOnly parseEnv "var1=val1\nvar2=val2")
    )
    , TestCase (
        assertEqual "no env vars"
        (Right M.empty)
        (AP.parseOnly parseEnv "")
    )
    , TestCase (
        assertEqual "no equals sign, no items"
        (Right M.empty)
        (AP.parseOnly parseEnv "var1val1var2val3")
    )
    , TestCase (
        Right M.empty @=? AP.parseOnly parseEnv "var1val1\nvar2val2"
    )
    ]
