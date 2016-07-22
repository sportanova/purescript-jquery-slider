module JQSlider
  (
    JQSlider
  , JQSLIDER
  , JQSLIDER_INIT
  , JQSLIDER_GET_VALUE
  , EventWithUi
  , Ui
  , initSlider
  , getValue
  , onChange
  )
  where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.JQuery (JQueryEvent, JQuery)
import DOM (DOM)
import Data.Foreign.Callback (Callback2, callback2)
import Data.Function (Fn1, Fn2, Fn7, runFn1, runFn2, runFn7)
import Prelude (Unit)

foreign import data JQSLIDER :: !
foreign import data JQSLIDER_INIT :: !
foreign import data JQSLIDER_GET_VALUE :: !

foreign import data JQSlider :: *

type EventWithUi = { event :: JQueryEvent, ui :: Ui }
type Ui = { handle :: JQSlider, value :: Int }



foreign import initSliderImpl :: forall e. Fn7 JQuery Int Int Int String String (Callback2 JQueryEvent Ui Unit)
  (Eff (dom :: DOM, jqSliderInit :: JQSLIDER_INIT | e) JQSlider)

initSlider :: forall a e. JQuery -> Int -> Int -> Int -> String -> String -> (JQueryEvent -> Ui -> Eff a Unit) ->
  (Eff (dom :: DOM, jqSliderInit :: JQSLIDER_INIT | e) JQSlider)
initSlider jqEl min max value orientation range change = runFn7 initSliderImpl jqEl min max value orientation range (callback2 change)




foreign import onChangeImpl :: forall eff a.
  Fn2 JQSlider
  (EventWithUi -> Eff (jqSlider :: JQSLIDER | eff) a)
  (Eff (jqSlider :: JQSLIDER | eff) Unit)

onChange :: forall eff a.
     JQSlider
  -> (EventWithUi -> Eff (jqSlider :: JQSLIDER | eff) a)
  -> Eff (jqSlider :: JQSLIDER | eff) Unit
onChange slider fn = runFn2 onChangeImpl slider fn




foreign import getValueImpl :: forall e. Fn1 JQSlider (Eff (dom :: DOM, jqSliderGetValue :: JQSLIDER_GET_VALUE | e) Int)

getValue :: forall e. JQSlider -> (Eff (dom :: DOM, jqSliderGetValue :: JQSLIDER_GET_VALUE | e) Int)
getValue slider = runFn1 getValueImpl slider
