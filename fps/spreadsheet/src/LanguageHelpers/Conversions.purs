module LanguageHelpers.Conversions where

import Prelude

-- Int to String
itos :: Int -> String
--itos = toString <<< toNumber
itos = show

