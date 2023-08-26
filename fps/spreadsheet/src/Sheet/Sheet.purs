module Sheet where

import Prelude

import Data.Tuple as T
import Matrix (Matrix(..), empty, isEmpty, height, width) as M
import Primitives (Val(..))

data CellState a = CellState (M.Matrix a)
--instance Show (CellState Val) where
--  show (CellState (mtx))
--    | M.isEmpty mtx = "(Empty Cell Matrix)"
--    | otherwise = "(Cell Matrix of size " <> show (M.width mtx) <> "x" <> show (M.height mtx) <> ")"
--instance Show (CellState String) where
--  show (CellState cs) = "(h:" <> show (M.height cs) <> ", w:" <> show (M.width cs) <> ")"


instance Show (CellState Val) where
  show (CellState (mtx))
    | M.isEmpty mtx = "(Empty Cell Matrix)"
    | otherwise = "(Cell Matrix of size " <> show (M.width mtx) <> "x" <> show (M.height mtx) <> ")"
instance Show (CellState String) where
  show (CellState cs) = "(h:" <> show (M.height cs) <> ", w:" <> show (M.width cs) <> ")"

newtype CRow = Row Int
newtype CCol = Cell Int

type CellLocation = T.Tuple CRow CCol

type Sheet a = {
    name :: String
    , rows :: Int
    , cols :: Int
    , cellState :: CellState a
    , selectedCell :: CellLocation
  }

getSheet :: forall a. Sheet a
getSheet =
  { name: ""
  , rows: 0
  , cols: 0
  , cellState: CellState (M.empty :: M.Matrix a)
  , selectedCell: T.Tuple (Row 0) (Cell 0)
  }
