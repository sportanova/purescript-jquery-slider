module HalogenExample where

import Prelude
import Halogen
import Halogen as H
import Data.Generic
import Data.Maybe
import Control.Monad.Aff
import Data.Foreign.Class
import Halogen.HTML.Events.Indexed as E
import Halogen.HTML.Indexed as HI
import Halogen.HTML.Properties.Indexed as PI
import Control.Monad.Eff (Eff)
import Halogen.HTML (ClassName, className, div_, li)
import Halogen.Util (awaitBody, runHalogenAff)
import Data.Foreign hiding (isNull,isArray)
import JQSlider
import JQSlider as JQS
import Debug.Trace
import Data.Maybe
import Control.Monad.Eff.JQuery (JQueryEvent, JQuery, select)

type State = { slider :: Maybe JQSlider }

data Query a =
    Initialize a
  | SetValue Int a
  | SliderChanged Int a
  | Sliding Int a
  | Finalize a

ui :: forall eff. Component State Query (ExEff eff)
ui = lifecycleComponent {
  render,
  eval,
  initializer: Just (action Initialize),
  finalizer: Just (action Finalize)
} where

  render :: State -> ComponentHTML Query
  render st = HI.div
    []
    [
      HI.div
        [ PI.id_ "slider" ]
        []
    , HI.button
        [ E.onClick (E.input_ (SetValue 143)) ]
        [ HI.text $ "Set to 143" ]
    ]

  eval :: Natural Query (ComponentDSL State Query (ExEff eff))
  eval (SetValue value next) = do
    { slider: sliderMay } <- get
    case sliderMay of
      Just slider -> fromEff $ setValue slider 143
      Nothing -> pure unit
    pure next
  eval (Initialize next) = do
    sliderEl <- fromEff $ select "#slider"
    slider <- fromEff $ initSlider sliderEl sliderConfig
    modify (_ { slider = Just slider })
    H.subscribe $ H.eventSource (onChange slider) \{event: e, ui: obj } -> do
      pure $ H.action (SliderChanged obj.value)
    H.subscribe $ H.eventSource (onSlide slider) \{event: e, ui: obj } -> do
      pure $ H.action (Sliding obj.value)
    pure next
  eval (SliderChanged value next) = trace ("changed: " ++ (show value)) \_ -> pure next
  eval (Sliding value next) = trace ("sliding: " ++ (show value)) \_ -> pure next
  eval (Finalize next) = pure next

sliderConfig :: SliderConfig
sliderConfig = {
  min: 1,
  max: 500,
  value: 250,
  orientation: "horizontal",
  range: "min"
}



data Slot = Slot

derive instance slotGeneric :: Generic Slot

instance eqSlot :: Eq Slot where
  eq = gEq

instance ordGeneric :: Ord Slot where
  compare = gCompare

type ExEff eff = Aff (EX eff)
type EX eff = HalogenEffects (
    jqSlider :: JQSLIDER
  , jqSliderInit :: JQSLIDER_INIT
  , jqSliderGetValue :: JQSLIDER_GET_VALUE
  , jqSliderSetValue :: JQSLIDER_SET_VALUE
  | eff
)

main :: forall eff. Eff (EX eff) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui { slider: Nothing} body
