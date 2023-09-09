module Interfaces.Sheet where

import Data.Argonaut (Json, JsonDecodeError, decodeJson)
import Data.Either (Either)

type Sheets = Array String

sheetsFromJson :: Json -> Either JsonDecodeError Sheets
sheetsFromJson = decodeJson
