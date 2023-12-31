module Main where

import Data.Maybe
import Halogen.Aff
import Prelude

import Component.Sheet (sheetC)
import Control.Monad.State (class MonadState)
import Data.Argonaut (Json, decodeJson, jsonEmptyString, parseJson)
import Data.Either (Either(..))
import Data.Number (fromString)
import Data.Set as S
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Halogen (liftAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Interfaces.Sheet (sheetsFromRequestText)
import Matrix (isEmpty, repeat, toIndexedArray, modify, get) as M
import Menu (Output(..), getState, menuC) as Menu
import Milkis as M
import Milkis.Impl.Window as MW
import Primitives (Val(..))
import Sheet (ActiveSheet(..), CellState(..), Sheet, getSheet)
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

myRec :: { name :: String, age :: Int }
myRec = {
  name: "matt"
  , age: 28
  }

type Slots = (
  sheet :: forall query. H.Slot query Void Unit
  , menu :: forall query. H.Slot query Menu.Output Unit
) -- TODO menu slot, use menu component

_sheet = Proxy :: Proxy "sheet"
_menu = Proxy :: Proxy "menu"

type State = { activeSheet :: ActiveSheet
  , loading :: Boolean
  , sheetFilenames :: Maybe (Array String)
  }
--instance Show State where
--  show { activeSheet: sheet} = "(State " <> show sheet <> ")"

getState :: State
getState = { activeSheet: NewDocument
  , loading: false
  , sheetFilenames: Nothing
  }

matrixSize = 5 :: Int

initialState :: forall input. input -> State
initialState _ = getState

component :: forall query input output. H.Component query input output Aff
--component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval {
      handleAction = handleAction
      , initialize = Just Initialize
      }
    }

data Action = Initialize | Finalize | HandleDocumentSelected Menu.Output

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.classes [HH.ClassName "w-full h-full flex flex-col justify-center items-stretch"]
    ]
    [
    HH.div
      [HP.classes
        [HH.ClassName "flex-shrink bg-red-200"
        ]
      ]
      [ HH.slot _menu unit Menu.menuC (getSheetsForMenu state.sheetFilenames) HandleDocumentSelected
      ]
    , HH.div_
      [ HH.slot_ _sheet unit sheetC { sheetName: state.activeSheet }
      ]
    ]

getSheetsForMenu :: Maybe (Array String) -> S.Set String
getSheetsForMenu Nothing = S.empty
getSheetsForMenu (Just sheets) = S.fromFoldable sheets

handleAction :: forall slots o m. MonadAff m => Action -> H.HalogenM State Action Slots o m Unit
handleAction action = case action of
  Initialize -> do
    _response <- H.liftAff $ (attempt $ M.fetch MW.windowFetch (M.URL "/api/sheets") M.defaultFetchOptions)
    asTxt <- H.liftAff $ case _response of
      Left err -> do
        H.liftEffect $ log "error converting to text"
        pure ""
      Right res -> do
        _asTxt <- M.text res
        H.liftEffect $ log ("to text success:" <> _asTxt)
        pure _asTxt
    let shts = sheetsFromRequestText asTxt
    H.modify_ \state -> state { sheetFilenames=Just shts }
    liftEffect $ log "component initialized"
  Finalize -> do
    --s <- H.get
    --H.put s
    liftEffect $ log "component finalized"
  HandleDocumentSelected menuOutput -> do
    case menuOutput of
      Menu.DocumentSelected newFilename -> do
        liftEffect $ log $ "doc changed " <> newFilename
        H.modify_ \s -> s { activeSheet=SheetId newFilename }
