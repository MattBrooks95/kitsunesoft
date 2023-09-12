module Interfaces.Sheet where

import Prelude
import Data.Either
import Data.Argonaut (Json, JsonDecodeError, decodeJson, parseJson)
import Data.Either (Either)

type Sheets = Array String

sheetsFromRequestText :: String -> Sheets
sheetsFromRequestText txt =
  let parseResult = do
                      asJson <- parseJson txt
                      decodeJson asJson
  in case parseResult of
    Left e -> []
    Right shts -> shts
