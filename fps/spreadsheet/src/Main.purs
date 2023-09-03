module Main where

import Data.Maybe
import Halogen.Aff
import Prelude

import Component.Sheet (sheetC)
import Control.Monad.State (class MonadState)
import Data.Number (fromString)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Matrix (isEmpty, repeat, toIndexedArray, modify, get) as M
import Menu (menuC, getState) as Menu
import Primitives (Val(..))
import Sheet (CellState(..), Sheet, getSheet)
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
  , menu :: forall query. H.Slot query Void Unit
) -- TODO menu slot, use menu component

_sheet = Proxy :: Proxy "sheet"
_menu = Proxy :: Proxy "menu"

type State = { activeSheet :: Sheet Val
  }
--instance Show State where
--  show { activeSheet: sheet} = "(State " <> show sheet <> ")"

getState :: State
getState = { activeSheet: (getSheet :: Sheet Val)
  }

matrixSize = 5 :: Int

--initialState :: forall input. input -> State
initialState _ = getState { activeSheet { cellState=CellState (M.repeat 5 5 (Letters "abc"))} }

--component :: forall query input output m. H.Component query input output Effect
--component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval {
      handleAction = handleAction
      --, initalize = Just Initialize
      --, finalize = Just Finalize
      }
    }

data Action = Initialize | Finalize

render :: forall m. State -> H.ComponentHTML Action Slots m
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
      [ HH.slot_ _menu unit Menu.menuC Menu.getState
      ]
    --HH.button [HE.onClick \_ -> Decrement ] [HH.text "-" ]
    --, HH.button [HE.onClick \_ -> Increment ] [HH.text "+" ]
    , HH.div
      [ HP.classes [HH.ClassName "bg-blue-200"]
      ]
      [ HH.slot_ _sheet unit sheetC { sheetName: "sheet1" }
      ]
    ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action Slots output m Unit
--handleAction :: forall output. Action -> H.HalogenM State Action Slots output Effect Unit
handleAction action = case action of
  Initialize ->
    log "component initialized"
  Finalize ->
    log "component finalized"
