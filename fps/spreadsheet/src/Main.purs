module Main where

import Data.Maybe
import Prelude

import Component.Sheet (sheetC)
import Control.Monad.State (class MonadState)
import Data.Number (fromString)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Type.Prelude (Proxy(..))
import Matrix (isEmpty, repeat, toIndexedArray, modify, get) as M

import Primitives (Val(..))
import Sheet (CellState(..), Sheet, getSheet)
import Menu (menuC)

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

type Slots = (
  sheet :: forall query. H.Slot query Void Unit
)

_sheet = Proxy :: Proxy "sheet"

type State = { activeSheet :: Sheet Val
  }
--instance Show State where
--  show { activeSheet: sheet} = "(State " <> show sheet <> ")"

getState :: State
getState = { activeSheet: (getSheet :: Sheet Val)
  }

matrixSize = 5 :: Int

initialState :: forall input. input -> State
initialState _ = getState { activeSheet { cellState=CellState (M.repeat 5 5 (Letters "abc"))} }

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
    }

render :: forall m action. State -> H.ComponentHTML action Slots m
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
      [ HH.text "menu"
      ]
    --HH.button [HE.onClick \_ -> Decrement ] [HH.text "-" ]
    --, HH.button [HE.onClick \_ -> Increment ] [HH.text "+" ]
    , HH.div
      [ HP.classes [HH.ClassName "bg-blue-200"]
      ]
      [ HH.slot_ _sheet unit sheetC { sheetName: "sheet1" }
      ]
    ]



