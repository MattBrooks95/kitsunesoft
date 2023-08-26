module Main where

import Data.Maybe
import Prelude

import Control.Monad.State (class MonadState)
import Data.Array (concat, concatMap, zip)
import Data.Number (fromString)
import Data.List as L
import Data.Tuple as T
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Matrix (Matrix(..), height, isEmpty, repeat, rows, toIndexedArray, width, modify, empty, get) as M


main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

newtype Row = Row Int
newtype Cell = Cell Int

type CellLocation = T.Tuple Row Cell

type Sheet a = {
    name :: String
    , rows :: Int
    , cols :: Int
    , cellState :: CellState a
    , selectedCell :: CellLocation
  }
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

myRec :: { name :: String, age :: Int }
myRec = {
  name: "matt"
  , age: 28
  }

getSheet :: forall a. Sheet a
getSheet =
  { name: ""
  , rows: 0
  , cols: 0
  , cellState: CellState (M.empty :: M.Matrix a)
  , selectedCell: T.Tuple (Row 0) (Cell 0)
  --, cellState: CellState (M.empty :: M.Matrix a)
  }

type State = { activeSheet :: Sheet Val
  }
--instance Show State where
--  show { activeSheet: sheet} = "(State " <> show sheet <> ")"

getState :: State
getState = { activeSheet: (getSheet :: Sheet Val)
  }

data Action = SetText String Int Int

data CellState a = CellState (M.Matrix a)
instance Show (CellState Val) where
  show (CellState (mtx))
    | M.isEmpty mtx = "(Empty Cell Matrix)"
    | otherwise = "(Cell Matrix of size " <> show (M.width mtx) <> "x" <> show (M.height mtx) <> ")"

data Val = Numeric Number | Letters String

instance Show (CellState String) where
  show (CellState cs) = "(h:" <> show (M.height cs) <> ", w:" <> show (M.width cs) <> ")"

matrixSize = 5 :: Int

initialState :: forall input. input -> State
initialState _ = getState { activeSheet { cellState=CellState (M.repeat 5 5 (Letters "abc"))} }

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div
    [ HP.classes [HH.ClassName "w-full h-full flex flex-col justify-center items-stretch"]
    ]
    [
    HH.div [HP.classes [HH.ClassName "flex-shrink bg-red-200"]] [ HH.text "menu" ]
    --HH.button [HE.onClick \_ -> Decrement ] [HH.text "-" ]
    --, HH.button [HE.onClick \_ -> Increment ] [HH.text "+" ]
    , HH.div [HP.classes [HH.ClassName "bg-blue-200"]] [
        --HH.text $ show state.activeSheet.cellState
        --, renderState state.activeSheet.cellState
        renderState state.activeSheet.cellState
        ]
    ]

--{getCellState $ M.repeat matrixSize matrixSize "text"

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  --handleAction = case _ of
  --  Increment -> H.modify_ \state -> state + 2
  --  Decrement -> H.modify_ \state -> state - 1

-- w -> "widget", describes what components can be used in the html
-- i -> "input", the type used to handle DOM events
--renderState :: forall w i. CellState Val -> HH.HTML w i
renderState (CellState matrix) =
  HH.div
    [
      HP.classes [ HH.ClassName "grid gap-0.5" ]
    ]
    (
      (renderInputs matrix) <>
      [
        HH.span_ [ HH.text "changed" ]
      ]
    )

renderInput value row col =
  let valAsString =
        case value of
          Letters str -> str
          Numeric num -> show num
  in
  --HH.span_ [ HH.text text]
  HH.input
    [ HP.value valAsString
    , HE.onValueInput (\str -> SetText str row col)
    ]

--renderInputs :: forall w i. M.Matrix Val -> Array (HH.HTML w i)
renderInputs m
  | M.isEmpty m = []
  | otherwise =
    let asArray = M.toIndexedArray m
    in
      map (\{value: v, x: row, y: col} -> renderInput v row col) asArray

handleAction :: forall m98. MonadState State m98 => Action -> m98 Unit
handleAction action = case action of
  SetText newText row col ->
    H.modify_ (\oldState@(stRecord@{ activeSheet: oldSheet@({cellState: (CellState matrix) })}) ->
      case M.get row col matrix of
        Just (Letters _) -> case M.modify row col (\_ -> Letters newText) matrix of
          Nothing -> oldState
          Just newMatrix -> oldState { activeSheet { cellState=CellState newMatrix } }
        Just (Numeric _) -> case fromString newText of
          Nothing -> oldState
          Just asNumber -> case M.modify row col (\_ -> Numeric asNumber) matrix of
            Just newMatrix -> oldState { activeSheet { cellState=CellState newMatrix } }
            Nothing -> oldState
        Nothing -> oldState
      --case M.modify row col (\_ -> Letters newText) matrix of
      --  -- how do I do the record syntax update to update just the cell matrix
      --  -- oldState { activeSheet { cellState = (CellState newMatrix) } }
      --  Just newMatrix ->
      --    let
      --      newCellState = CellState newMatrix :: CellState Val
      --      newSheet = oldSheet :: Sheet Val
      --    in
      --      oldState { activeSheet { cellState=newCellState } }
      --    --oldState {
      --    --  activeSheet=(oldSheet :: Sheet Val)
      --    --  }
      --  -- TODO if the indices into the matrix were out of bounds, the modify function
      --  -- will return nothing. this should never happen because I check the bounds
      --  -- I need to break the habit of coercing w/fromJust when I think the program should crash
      --  -- because of a programmer mistake
      --  Nothing -> oldState
        )
