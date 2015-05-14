
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
      this.$el.html(''
        + this.button('red', 'Red')
        + this.button('blue', 'Blue')
        + this.button('green', 'Green')
        );
      this.model.on('change', this.render, this);

      var self = this;
      _.map(['red', 'blue', 'green'], function (color) {
        self.$('#btn-' + color).click(function () { self.click(color) })
      });
    },

    render: function () {
      var mdl = this.model.toJSON();
      var self = this;
      _.map(['red', 'blue', 'green'], function (color) {
        var c = mdl[color];
        var el = self.$('#btn-' + color);
        if (c > 0) {
          el.removeClass('btn-default')
            .addClass('btn-success');
        } else {
          el.removeClass('btn-success')
            .addClass('btn-default');
        }
      });
    },

    click : function (color) {
      var mdl = this.model.toJSON();
      if (mdl[color] > 0) {
        mdl[color] = 0;
      } else {
        mdl[color] = 8;
      }
      this.model.set(mdl);
      this.model.save();
    },

    button: function (id, text) {
        return ('<button type="button" class="btn btn-default" id="btn-'
              + id +'">'
              + text + '</button>');
    }
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




