module Example where

import Prelude
import Control.Monad.Eff
import JQSlider
import Control.Monad.Eff.JQuery (JQueryEvent, JQuery, select)
import Debug.Trace

main = do
  sliderEl <- select "#slider"
  slider <- initSlider sliderEl 1 500 250 "horizontal" "min" printValue
  getValue slider

printValue :: forall a. JQueryEvent -> Ui -> Eff a Unit
printValue jqEvent obj = trace (show obj.value) \_ -> return unit
