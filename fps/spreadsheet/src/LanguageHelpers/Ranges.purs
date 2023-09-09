module LanguageHelpers.Ranges where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Char (fromCharCode)
import Data.Array ((..))
import Data.String (singleton, codePointFromChar)

getCharacters :: Int -> Int -> Array Char
getCharacters start end = map (fromMaybe ' ' <<< fromCharCode) (start..end)

strFromC :: Char -> String
strFromC symbol = singleton $ codePointFromChar symbol
