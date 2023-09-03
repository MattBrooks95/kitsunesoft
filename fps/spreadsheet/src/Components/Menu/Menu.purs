module Menu where

import Prelude

import Data.Array (fromFoldable)
import Data.Functor (
  map
  ) as F 

import Data.Set as S
import Data.Maybe
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE

data Action = SelectFile String

type FileState = {
  active :: String
  , available :: S.Set String
}

getAvailableArray :: FileState -> Array String
getAvailableArray { available } = fromFoldable available

type Input = State

type State = {
  fileState :: FileState
}

getState :: State
getState = {
  fileState: {
      available: S.singleton defaultDocument
      , active: defaultDocument
    }
  }

menuC =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

defaultDocument :: String
defaultDocument = "new document"

initialState :: Input -> State
initialState state = state

render :: forall w. State -> HH.HTML w Action
render menuState =
  HH.div
    [
      HP.classes [ HH.ClassName "flex flex-row items-center w-full" ]
    ]
    [
      HH.select
      [
        HP.value menuState.fileState.active
        , HE.onValueChange $ (\newVal -> SelectFile newVal)
      ]
      let filenames = (getAvailableArray menuState.fileState) in F.map mkOption filenames
    ]

mkOption :: String -> forall w i. HH.HTML w i
mkOption val = HH.option [ HP.value val] [ HH.text val ]

--type copy pasta'd from https://purescript-halogen.github.io/purescript-halogen/guide/02-Introducing-Components.html
--not sure how I can learn to write these by hand, there will eventually be a case where
--I have to update the state and do an IO effect of some sort
handleAction :: forall output m. Action -> H.HalogenM State Action () output m Unit
handleAction action = case action of
  SelectFile newFilename ->
    H.modify_ (\oldState@({ fileState: fs }) -> oldState { fileState=fs { active=newFilename }})
