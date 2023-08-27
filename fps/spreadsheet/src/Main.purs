module Main where

import Data.Maybe
import Prelude

import Control.Monad.State (class MonadState)
import Data.Number (fromString)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Matrix (isEmpty, repeat, toIndexedArray, modify, get) as M
import Sheet (CellState(..), Sheet, getSheet)
import Primitives (Val(..))

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body


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


type State = { activeSheet :: Sheet Val
  }
--instance Show State where
--  show { activeSheet: sheet} = "(State " <> show sheet <> ")"

getState :: State
getState = { activeSheet: (getSheet :: Sheet Val)
  }

data Action = SetText String Int Int

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

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

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
  SetText newText row col -> H.modify_ (\oldState ->
    case updateCellFromInput oldState newText row col of
      Nothing -> oldState
      Just newState -> newState
    )
    --H.modify_ (\oldState@(stRecord@{ activeSheet: oldSheet@({cellState: (CellState matrix) })}) ->
    --  case M.get row col matrix of
    --    Just (Letters _) -> case M.modify row col (\_ -> Letters newText) matrix of
    --      Nothing -> oldState
    --      Just newMatrix -> oldState { activeSheet { cellState=CellState newMatrix } }
    --    Just (Numeric _) -> case fromString newText of
    --      Nothing -> oldState
    --      Just asNumber -> case M.modify row col (\_ -> Numeric asNumber) matrix of
    --        Just newMatrix -> oldState { activeSheet { cellState=CellState newMatrix } }
    --        Nothing -> oldState
    --    Nothing -> oldState
    --    )

updateCellFromInput :: State -> String -> Int -> Int -> Maybe State
updateCellFromInput oldState@{activeSheet: { cellState: CellState mtx }} input row col = do
  currCell <- M.get row col mtx
  newMatrix <- case currCell of
    Letters _ -> M.modify row col (\_ -> Letters input) mtx
    Numeric _ -> case fromString input of
      Just myNum -> M.modify row col (\_ -> Numeric myNum) mtx
      Nothing -> Nothing
  pure $ oldState { activeSheet { cellState = CellState newMatrix } }
