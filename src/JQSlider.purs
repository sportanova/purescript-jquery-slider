module JQSlider
  (
    JQSlider
  , JQSLIDER
  , JQSLIDER_INIT
  , JQSLIDER_GET_VALUE
  , JQSLIDER_SET_VALUE
  , EventWithUi
  , Ui
  , SliderConfig
  , initSlider
  , getValue
  , setValue
  , onChange
  , onSlide
  )
  where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (JQueryEvent, JQuery)
import DOM (DOM)
import Data.Function (Fn1, Fn2, runFn1, runFn2)
import Prelude (Unit)

foreign import data JQSLIDER :: !
foreign import data JQSLIDER_INIT :: !
foreign import data JQSLIDER_GET_VALUE :: !
foreign import data JQSLIDER_SET_VALUE :: !

foreign import data JQSlider :: *

type EventWithUi = { event :: JQueryEvent, ui :: Ui }
type Ui = { handle :: JQSlider, value :: Int }

type SliderConfig = {
  min :: Int,
  max :: Int,
  value :: Int,
  orientation :: String,
  range :: String
}


type SliderFn2Impl eff a =
  Fn2 JQSlider
  (EventWithUi -> Eff (jqSlider :: JQSLIDER | eff) a)
  (Eff (jqSlider :: JQSLIDER | eff) Unit)

type SliderFn2 eff a = JQSlider
  -> (EventWithUi -> Eff (jqSlider :: JQSLIDER | eff) a)
  -> Eff (jqSlider :: JQSLIDER | eff) Unit



foreign import initSliderImpl :: forall e. Fn2 JQuery SliderConfig
  (Eff (dom :: DOM, jqSliderInit :: JQSLIDER_INIT | e) JQSlider)

initSlider :: forall e. JQuery -> SliderConfig ->
  (Eff (dom :: DOM, jqSliderInit :: JQSLIDER_INIT | e) JQSlider)
initSlider jqEl config = runFn2 initSliderImpl jqEl config




foreign import onChangeImpl :: forall eff a. SliderFn2Impl eff a

onChange :: forall eff a. SliderFn2 eff a
onChange slider fn = runFn2 onChangeImpl slider fn




foreign import onSlideImpl :: forall eff a. SliderFn2Impl eff a

onSlide :: forall eff a. SliderFn2 eff a
onSlide slider fn = runFn2 onSlideImpl slider fn




foreign import getValueImpl :: forall e. Fn1 JQSlider (Eff (dom :: DOM, jqSliderGetValue :: JQSLIDER_GET_VALUE | e) Int)

getValue :: forall e. JQSlider -> (Eff (dom :: DOM, jqSliderGetValue :: JQSLIDER_GET_VALUE | e) Int)
getValue slider = runFn1 getValueImpl slider




foreign import setValueImpl :: forall e. Fn2 JQSlider Int (Eff (dom :: DOM, jqSliderSetValue :: JQSLIDER_SET_VALUE | e) Unit)

setValue :: forall e. JQSlider -> Int -> (Eff (dom :: DOM, jqSliderSetValue :: JQSLIDER_SET_VALUE | e) Unit)
setValue slider value = runFn2 setValueImpl slider value
