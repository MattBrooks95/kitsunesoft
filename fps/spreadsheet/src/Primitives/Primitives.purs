module Primitives where

import Data.Show (class Show, show)

data Val = Numeric Number | Letters String

instance Show Val where
  show (Numeric num) = show num
  show (Letters str) = str
