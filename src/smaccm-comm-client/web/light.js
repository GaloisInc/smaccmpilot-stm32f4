
$(function() {

  var RgbLed = Backbone.Model.extend({
    urlRoot: '/controllable_vehicle_i/rgb_led',
    defaults: {
      red: 0,
      blue: 0,
      green: 0
    },
  });

  var RgbLedView = Backbone.View.extend({
    initialize: function () {
      var self = this;
      this.$el.spectrum({
        flat: true,
        color: '#000',
        change: function (color) {
          var rgb = color.toRgb();
          self.model.set({
            red:   rgb.r / 16,
            green: rgb.g / 16,
            blue:  rgb.b / 16
          });
          self.model.save();
        }
      })
      this.model.on('change', this.render, this);

    },

    render: function () {
      var mdl = this.model.toJSON();
      this.$el.spectrum('set', tinycolor({
          r : mdl.red * 16,
          g : mdl.green * 16,
          b : mdl.blue * 16,
          a : 1
      }));

    },

  });

  window.rgbLed =
    new RgbLed({});
  window.rgbLedView =
    new RgbLedView({ model: rgbLed, el: '#rgbled-view' });

  window.rgbLedScheduler =
    new Scheduler({ period: false}, rgbLed);
  window.rgbLedSchedulerView =
    new SchedulerButtonView({ model: rgbLedScheduler, el: '#rgbled-sch-btn' });

});




