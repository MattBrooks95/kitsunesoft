module Sheet where

import Prelude

import Data.Array (concat, length, replicate, snoc, take)
import Data.Foldable (foldl, maximum)
import Data.List (Pattern(..))
import Data.Maybe (Maybe(..))
import Data.String (split, Pattern(..), length) as S
import Data.Tuple as T
import Debug (trace)
import Matrix (Matrix(..), empty, isEmpty, height, width, fromArray) as M
import Primitives (Val(..))

data CellState a = CellState (M.Matrix a)
--instance Show (CellState Val) where
--  show (CellState (mtx))
--    | M.isEmpty mtx = "(Empty Cell Matrix)"
--    | otherwise = "(Cell Matrix of size " <> show (M.width mtx) <> "x" <> show (M.height mtx) <> ")"
--instance Show (CellState String) where
--  show (CellState cs) = "(h:" <> show (M.height cs) <> ", w:" <> show (M.width cs) <> ")"
getCells :: forall a. CellState a -> M.Matrix a
getCells (CellState cells) = cells


instance Show (CellState Val) where
  show (CellState (mtx))
    | M.isEmpty mtx = "(Empty Cell Matrix)"
    | otherwise = "(Cell Matrix of size " <> show (M.width mtx) <> "x" <> show (M.height mtx) <> ")"
instance Show (CellState String) where
  show (CellState cs) = "(h:" <> show (M.height cs) <> ", w:" <> show (M.width cs) <> ")"

newtype CRow = Row Int
newtype CCol = Col Int

type CellLocation = T.Tuple CRow CCol

type Sheet a = {
    rows :: Int
    , cols :: Int
    , cellState :: CellState a
    , selectedCell :: CellLocation
  }

getSheet :: forall a. Sheet a
getSheet =
  { rows: 0
  , cols: 0
  , cellState: CellState (M.empty :: M.Matrix a)
  , selectedCell: T.Tuple (Row 0) (Col 0)
  }

getSheetFromText :: String -> Sheet Val
getSheetFromText sheetContents =
  let
    rows = (S.split (S.Pattern "\n") sheetContents)
    cellsAsStrings = map (S.split (S.Pattern ",")) rows
    numRows = trace ("getSheetFromText" <> show rows <> " cells as strings:" <> show cellsAsStrings) \_ -> length rows
    numCols =
      case maximum $ map length cellsAsStrings of
        Nothing -> 0
        Just len -> len
    matrixDim = trace ("numRows:" <> show numRows <> ", numCols:" <> show numCols) \_ -> getMatrixDim numRows numCols
    -- TODO ensure that all the rows are of the same length, and numRows == numCols
    -- the Matrix code I'm using has that requirement, which doesn't match with up with spreadsheet data
    -- which could just be one row with many columns, maybe a refactor
    -- TODO need to turn these strings into Vals (formula or string or number???)
    madeSquare = foldl (\acc item -> snoc acc (normalizeRow matrixDim item)) ([] :: Array (Array String)) (trace cellsAsStrings \_ -> cellsAsStrings) :: Array (Array String)
    newCellState = 
      case M.fromArray madeSquare of
        Nothing -> CellState M.empty
        Just newMatrix ->
          -- TODO just going to hard code them all as strings for now
          -- but need to differentiate between date, string, number and formula
          CellState $ map Letters newMatrix
  in
    { rows: M.height (getCells newCellState)
      , cols: M.width (getCells newCellState)
      , cellState: (trace (show newCellState <> " made square:" <> show madeSquare) \_ -> newCellState)
      , selectedCell: T.Tuple (Row 0) (Col 0)
    }

normalizeRow :: Int -> Array String -> Array String
normalizeRow targetSize row =
  let
    rowLen = length row
  in
  if rowLen < trace ("target size:" <> show targetSize <> " row len:" <> show rowLen) \_ -> targetSize
  then concat [row, replicate (targetSize - rowLen) ""]
  -- this take should be unnecessary, if you properly measured the length of
  -- the longest row and passed it into this function
  else take targetSize row

getMatrixDim :: Int -> Int -> Int
getMatrixDim = max

--instance Show (Sheet Val) where
--  show ({ name: sheetName, rows: numRows, cols: numCols, cellState: cellS }) =
--    "(Sheet name:"
--    <> sheetName
--    <> " (rows:"
--    <> show numRows
--    <> ", cols:"
--    <> show numCols
--    <> ")"
--    <> show cellS
--    <> ")"

data ActiveSheet = SheetId String | NewDocument

sheetDisplayName :: ActiveSheet -> String
sheetDisplayName NewDocument = "new document"
sheetDisplayName (SheetId sId) = sId

