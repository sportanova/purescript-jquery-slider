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
import Control.Monad.Eff.JQuery (JQueryEvent, JQuery, select)

type State = {}

data Query a =
    Initialize a
  | SliderChange Int a
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
    [ PI.id_ "slider" ]
    []

  eval :: Natural Query (ComponentDSL State Query (ExEff eff))
  eval (Initialize next) = do
    sliderEl <- fromEff $ select "#slider"
    slider <- fromEff $ initSlider sliderEl 1 500 250 "horizontal" "min" printValue
    H.subscribe $ H.eventSource (onChange slider) \{event: e, ui: obj } -> do
      pure $ H.action (SliderChange obj.value)
    pure next
  eval (SliderChange value next) = trace ("changed: " ++ (show value)) \_ -> pure next
  eval (Finalize next) = pure next

printValue :: forall a. JQueryEvent -> Ui -> Eff a Unit
printValue jqEvent obj = trace (show obj.value) \_ -> return unit


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
  | eff
)

main :: forall eff. Eff (EX eff) Unit
main = runHalogenAff do
  body <- awaitBody
  runUI ui {} body
