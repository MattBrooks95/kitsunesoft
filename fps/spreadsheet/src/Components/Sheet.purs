module Component.Sheet where

import Control.Monad.State (class MonadState)
import Data.Array (concat, foldl, (..), (:))
import Data.Either (Either(..))
import Data.Functor (map) as F
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Number (fromString)
import Data.Number.Format (toString)
import Data.Show (class Show)
import Data.Tuple as T
import Data.Unit (Unit)
import Debug (trace)
import Effect.Aff (attempt)
import Effect.Aff.Class (class MonadAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HtmlHelpers.Basic (mkSpan)
import LanguageHelpers.Ranges (getCharacters, strFromC)
import Matrix as M
import Milkis as MI
import Milkis.Impl.Window as MIW
import Prelude (bind, discard, map, pure, show, ($), (-), (<$>), (<<<), (<>), (==))
import Primitives (Val(..))
import Sheet (ActiveSheet(..), CRow, CellState(..), Sheet, getSheet, getSheetFromText, sheetDisplayName)

data Action = SetText String Int Int
  | Receive Input

type Input = {
    sheetName :: ActiveSheet
  }

type State = {
  sheet :: Maybe (Sheet Val)
  , sheetName :: ActiveSheet
  }

--instance Show (State) where
--  show { cellState: cs, sheetName: sn} = (cellStatePrint cs) <> sheetNamePrint
--    where
--      cellStatePrint Nothing = "cell state was Nothing"
--      cellStatePrint (Just (CellState matrix)) = "cell state exists, matrix with dims: (" <> M.width matrix <> "," <> M.height matrix <> ")"
--      sheetNamePrint NewDocument = "new document"
--      sheetNmaePrint (SheetId sName) = "document with name:" <> sName

getState :: State
getState = {
  sheet: Just getSheet
  , sheetName: NewDocument
  }

sheetC :: forall query output m. MonadAff m => H.Component query Input output m
sheetC =
  H.mkComponent
    { initialState
    , render: renderState
    , eval: H.mkEval $ H.defaultEval {
        handleAction = handleAction
        , receive = Just <<< Receive
        }
    }

initialState :: Input -> State
initialState ({ sheetName: sheetN }) =
  getState
    { sheet=Nothing -- TODO default to Nothing
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

--handleAction :: forall output m. MonadState State m => Action -> H.HalogenM State Action () output m Unit
handleAction action = case action of
  --SetText newText row col -> H.modify_ (\oldState -> updateCellFromInput oldState newText row col)
  SetText newText row col -> H.modify_ (\oldState -> oldState)
  Receive {sheetName: activeSheet} -> do
    case activeSheet of
      NewDocument -> do
        H.liftEffect $ log "new document given to sheet component"
      SheetId sn ->
        do 
          H.liftEffect $ log $ "received sheetname:" <> sn
          _response <- H.liftAff $ (attempt $ MI.fetch MIW.windowFetch (MI.URL ("/api/sheet" <> "/" <> sn)) MI.defaultFetchOptions)
          asTxt <- H.liftAff $ case _response of
            Left err -> do
              H.liftEffect $ log $ "error getting sheet with id:" <> sn
              -- H.modify_ (\s -> s { sheetName = "" })
              pure Nothing
              --pure ""
            Right res -> do
              _asTxt <- MI.text res
              H.liftEffect $ log $ "got sheet contents:" <> _asTxt
              pure (Just _asTxt)
          case asTxt of
            Nothing -> do
              H.liftEffect $ log "sheet contents was nothing"
            Just sheetContents ->
              H.modify_ (\oldState -> oldState {
                sheetName = SheetId sn
                , sheet = Just newSheetFromContent
                })
              where
              newSheetFromContent = getSheetFromText sheetContents

updateCellFromInput :: State -> String -> Int -> Int -> State
updateCellFromInput oldState@{ sheet: Nothing } input row col = oldState
updateCellFromInput oldState@{ sheet: oldSheet@(Just {cellState: oldCellState@(CellState mtx)}) } input row col =
  maybe oldState updateCellState newMatrix
  where
  updateCellState :: (M.Matrix Val) -> State
  updateCellState newM = oldState  -- TODO this needed to be updated because I changed the state type right now it is ignoring input
  newMatrix :: Maybe (M.Matrix Val)
  newMatrix =
    let currCell = M.get row col mtx
    in do
      newValue <- case currCell of
        Just (Letters _) -> Just $ Letters input
        Just (Numeric _) -> Numeric <$> fromString input
        _ -> Nothing
      M.modify row col (\_ -> newValue) mtx

-- w -> "widget", describes what components can be used in the html
-- i -> "input", the type used to handle DOM events
--renderState :: forall w i. CellState Val -> HH.HTML w i
renderState :: forall w. State -> HH.HTML w Action
renderState state =
  HH.div
    [
      HP.classes [ HH.ClassName "grid gap-0.5 flex-nowrap text-center" ]
    ]
    (concat [ F.map (mkSpan "row-start-1") (getColHeaders)
      , renderSheet (trace ("cell state for render rows") \_ -> state.sheet)
      , [ mkSpan "" (sheetDisplayName state.sheetName)]
    ])

renderSheet :: forall w. Maybe (Sheet Val) -> Array (HH.HTML w Action)
renderSheet Nothing = [ HH.span_ [ HH.text "no sheet" ] ]
renderSheet (Just sheet) = renderRows (Just sheet.cellState)

renderRows :: forall w. Maybe (CellState Val) -> Array (HH.HTML w Action)
renderRows Nothing = [ HH.span_ [ HH.text "no cells" ] ]
renderRows (Just (CellState matrix)) =
  let numRows = M.height matrix :: Int
      numCols = M.width matrix :: Int
      rows = 0..(numRows - 1) :: Array Int
      -- TODO the 0th index is used for drawing the row label,
      --so it's kinda hacky to add 1 to the number of columns to ensure that everything gets drawn
      --I think you need to make your own data structure that handles this
      --this means that when you get the data, you have to get at (cols - 1)
      cols = 0..(numCols) :: Array Int
  in
    --map (\r -> HH.span_ [ HH.text (toString (toNumber r)) ]) rows
    --rendering a 2d matrix as input elemnts
    --each row starts with a span that contains the row number
    trace ("renderRows, numRows" <> show numRows <> " num cols:" <> show numCols) \_ -> foldl (\acc row ->
        acc <> (map (\col -> 
          if col == 0
          then HH.span [ HP.classes [HH.ClassName "col-start-1 text-center"] ] [ HH.text (show row) ]
          else
            -- TODO the indices were flipped, I want them to be (row, col), but they were (col, row)
            -- so I just flipped them again XD
            --case M.get row col matrix of
            case M.get (col - 1) row matrix of
              Nothing -> HH.span_ [ HH.text ("error, row" <> show row <> " col" <> show col) ]
              Just val -> renderInput "w-24" val row col
          ) cols)
      ) [] rows

getColHeaders :: Array String
getColHeaders = "":map (strFromC) (getCharacters 65 90)

getRowHeaders :: Array String
getRowHeaders = map (toString <<< toNumber) (1..26)
