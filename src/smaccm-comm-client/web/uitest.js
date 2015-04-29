
$(function() {

  var UserInput = Backbone.Model.extend({
    urlRoot: '/controllable_vehicle_i/user_input',
    defaults: {
      ui: { throttle: 0, roll: 0, pitch: 0, yaw: 0 },
      source: 'Ppm',
      time: 0
    }
  });


  var UserInputSliderView = Backbone.View.extend({
    initialize: function (options) {
      this.axis = options.axis;
      this.model.on("change", this.render, this);
      this.render();
      this.$progbar = $('#ui-' + this.axis + '-progbar');
      this.$label   = $('#ui-' + this.axis + '-progbar-lbl');
    },
    render: function () {
      console.log(this.axis + ' ' + this.model.toJSON()['ui'][this.axis]);
    }
  });

  var UserInputView = function (opts) {
    this.throttle = new UserInputSliderView({ model: opts.model, axis: 'throttle' });
    this.roll     = new UserInputSliderView({ model: opts.model, axis: 'roll' });
    this.pitch    = new UserInputSliderView({ model: opts.model, axis: 'pitch' });
    this.yaw      = new UserInputSliderView({ model: opts.model, axis: 'yaw'  });
  };

  window.userInput = new UserInput({});
  window.userInputView = new UserInputView({ model: userInput });

});
