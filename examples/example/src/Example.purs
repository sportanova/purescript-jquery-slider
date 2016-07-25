module Example where

import Prelude
import Control.Monad.Eff
import JQSlider
import Control.Monad.Eff.JQuery (JQueryEvent, JQuery, select)
import Debug.Trace

main = do
  sliderEl <- select "#slider"
  slider <- initSlider sliderEl sliderConfig
  getValue slider

sliderConfig :: SliderConfig
sliderConfig = {
  min: 1,
  max: 500,
  value: 250,
  orientation: "horizontal",
  range: "min"
}
