module HtmlHelpers.Basic where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

mkSpan :: String -> String -> forall w i. HH.HTML w i
mkSpan addClass val = HH.span [ HP.classes [HH.ClassName addClass] ] [HH.text val]

mkOption :: String -> forall w i. HH.HTML w i
mkOption val = HH.option [ HP.value val] [ HH.text val ]
