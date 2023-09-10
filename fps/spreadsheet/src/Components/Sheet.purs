module Component.Sheet where

import Data.Maybe
import Primitives
import Sheet

import Control.Monad.State (class MonadState)
import Data.Array (catMaybes, concat, foldl, (..), (:))
import Data.Char (fromCharCode)
import Data.Functor (map) as F
import Data.Int (toNumber)
import Data.Number (fromString)
import Data.Number.Format (toString)
import Data.String (joinWith)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HtmlHelpers.Basic (mkSpan)
import LanguageHelpers.Ranges (getCharacters, strFromC)
import Matrix as M
import Prelude (Unit, bind, const, map, otherwise, pure, show, ($), (-), (<$>), (<<<), (<>), (==))

data Action = SetText String Int Int
  | Receive Input

type Input = {
    sheetName :: String
  }

type State = {
  cellState :: Maybe (CellState Val)
  , sheetName :: String
  }

getState :: State
getState = {
  cellState: Nothing
  , sheetName: ""
  }

sheetC :: forall query output m. H.Component query Input output m
sheetC =
  H.mkComponent
    { initialState
    , render: renderState
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: Input -> State
initialState ({ sheetName: sheetN }) =
  getState
    { cellState=Just $ CellState (M.repeat 5 5 (Letters "abc"))
    , sheetName=sheetN
    }

renderInput className value row col =
  let valAsString =
        case value of
          Letters str -> str
          Numeric num -> show num
  in
  --HH.span_ [ HH.text text]
  HH.input
    [ HP.value valAsString
    , HE.onValueInput (\str -> SetText str row col)
    , HP.classes [ HH.ClassName className ]
    ]

--handleAction :: forall m98 output m. MonadState State m98 => Action -> H.HalogenM State Action () output m Unit
handleAction action = case action of
  --SetText newText row col -> H.modify_ (\oldState -> updateCellFromInput oldState newText row col)
  SetText newText row col -> H.modify_ (\oldState -> oldState)
  Receive input -> H.modify_ (\oldState -> oldState { sheetName = input })

updateCellFromInput :: State -> String -> Int -> Int -> State
updateCellFromInput oldState@{ cellState: Nothing } input row col = oldState
updateCellFromInput oldState@{ cellState: oldCellState@(Just (CellState mtx)) } input row col =
  maybe oldState updateCellState newMatrix
  where
  updateCellState = \newM -> oldState { cellState=Just $ CellState newM }
  newMatrix :: Maybe (M.Matrix Val)
  newMatrix =
    let currCell = getCell row col mtx
    in do
      newValue <- case currCell of
        Just (Letters _) -> Just $ Letters input
        Just (Numeric _) -> Numeric <$> fromString input
        _ -> Nothing
      M.modify row col (\_ -> newValue) mtx
  getCell :: Int -> Int -> M.Matrix Val -> Maybe Val
  getCell = M.get
  --case M.get row col mtx of
  --  Nothing -> oldState
  --  Just currCell ->
  --    case currCell of
  --      Letters _ -> case M.modify row col (\_ -> Letters input) mtx of
  --        Nothing -> oldState
  --        Just newMatrix -> oldState { cellState = CellState newMatrix }
  --      Numeric _ -> case fromString input of
  --        Just myNum -> M.modify row col (\_ -> Numeric myNum) mtx
  --        Nothing -> Nothing

--renderInputs :: forall w i. M.Matrix Val -> Array (HH.HTML w i)
--renderInputs m =
--    let asArray = M.toIndexedArray m
--        numCols = M.width
--        numRows = M.height
--    in
--      map (\{value: v, x: row, y: col} -> renderInput (if col == 0 then Just "col-start-2" else Nothing) v row col) asArray

--if I go down this route it looks like I have to manually tell each row its
--row-start, which will not work. Maybe I need a nested grid?
--getClassesForCell :: Int -> Int -> String
--getClassesForCell row col = joinWith " " $ catMaybes [
--    if col == 0 then Just "col-start-2" else Nothing
--    , if row == 0 then Just "row-start-2" else Nothing
--  ]

-- w -> "widget", describes what components can be used in the html
-- i -> "input", the type used to handle DOM events
--renderState :: forall w i. CellState Val -> HH.HTML w i
renderState ({ cellState: cState }) =
  HH.div
    [
      HP.classes [ HH.ClassName "grid gap-0.5 flex-nowrap text-center" ]
    ]
    (concat [ F.map (mkSpan "row-start-1") (getColHeaders)
    , renderRows cState
      --, F.map (mkSpan "col-start-1") (getRowHeaders)
      --, (case cState of
      --    Nothing -> [ HH.span_ [ HH.text "no cells" ] ]
      --    Just (CellState matrix) -> (
      --      renderInputs matrix
      --      )
      --  )
      ])

renderRows :: forall w. Maybe (CellState Val) -> Array (HH.HTML w Action)
renderRows Nothing = [ HH.span_ [ HH.text "no cells" ] ]
renderRows (Just (CellState matrix)) =
  let numRows = M.height matrix :: Int
      numCols = M.width matrix :: Int
      rows = 0..(numRows - 1) :: Array Int
      cols = 0..(numCols - 1) :: Array Int
  in
    --map (\r -> HH.span_ [ HH.text (toString (toNumber r)) ]) rows
    foldl (\acc row ->
        acc <> (map (\col -> 
          if col == 0
          then HH.span [ HP.classes [HH.ClassName "col-start-1"] ] [ HH.text (show row) ]
          else
            case M.get row col matrix of
              Nothing -> HH.span_ [ HH.text ("error, row" <> show row <> " col" <> show col) ]
              Just val -> renderInput "" val row col
          ) cols)
      ) [] rows

    --case cState of
    --  Nothing -> [ HH.span_ [ HH.text "no cells" ] ]
    --  Just (CellState matrix) -> (
    --    if M.isEmpty matrix
    --    then []
    --    else
    --      (renderInputs matrix) <>
    --      [
    --        HH.span_ [ HH.text "changed" ]
    --      ]
    --      )

getColHeaders :: Array String
getColHeaders = "":map (strFromC) (getCharacters 65 90)

getRowHeaders :: Array String
getRowHeaders = map (toString <<< toNumber) (1..26)
