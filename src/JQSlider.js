// module JQSlider

exports.initSliderImpl = function($slider, min, max, value, orientation, range, changeFn) {
  return function() {
    if ($slider.length > 0) {
      return $slider.slider({
        min: min,
        max: max,
        value: value,
        orientation: orientation,
        range: range,
        change: changeFn
      });
    }
  }
}

exports.onChangeImpl = function ($slider, fn) {
  return function () {
    return $slider.on("slidechange", function (e, obj) {
      fn({ event: e, ui: obj })();
    });
  };
};

exports.getValueImpl = function($slider) {
  return function() {
    if ($slider.length > 0) {
      return $slider.slider( "option", "value" );
    }
  }
}
