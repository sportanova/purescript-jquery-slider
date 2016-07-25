// module JQSlider

exports.initSliderImpl = function($slider, config) {
  return function() {
    if ($slider.length > 0) {
      return $slider.slider({
        min: config.min,
        max: config.max,
        value: config.value,
        orientation: config.orientation,
        range: config.range
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

exports.onSlideImpl = function ($slider, fn) {
  return function () {
    return $slider.on("slide", function (e, obj) {
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

exports.setValueImpl = function($slider, value) {
  return function() {
    if ($slider.length > 0) {
      return $slider.slider( "option", "value", value );
    }
  }
}
