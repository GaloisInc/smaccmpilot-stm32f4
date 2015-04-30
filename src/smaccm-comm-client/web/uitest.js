
$(function() {

  var Scheduler = Backbone.Model.extend({
    defaults: {
      period: false
    },
    initialize: function (attrs, model) {
      this.model = model;
      this.interval = null;
      this.on('change', this.update, this);
      this.set(attrs);
    },
    update: function () {
      var attrs = this.toJSON();
      var self = this;
      if (this.interval) {
        clearInterval(this.interval);
      }
      if (attrs.period > 0) {
        this.interval = setInterval(function () { self.model.fetch() },
                                    attrs.period);
      } else {
        this.interval = null;
      }
    }
  });

  var SchedulerButtonView = Backbone.View.extend({
    events: {
      'click' : 'click'
    },
    initialize: function () {
      this.model.on('change', this.render, this);
      this.render();
    },
    render: function () {
      var p = this.model.get('period');
      if (p > 0) {
        this.$el.removeClass('btn-warning btn-success')
                .addClass('btn-success')
                .html('Polling');
      } else {
        this.$el.removeClass('btn-warning btn-success')
                .addClass('btn-warning')
                .html('Begin Polling');
      }
    },
    click: function () {
      var p = this.model.get('period');
      if (p > 0) {
        this.model.set('period', false);
      } else {
        this.model.set('period', 200);
      }
    }
  });

  var UserInput = Backbone.Model.extend({
    urlRoot: '/controllable_vehicle_i/user_input',
    defaults: {
      ui: { throttle: 0, roll: 0, pitch: 0, yaw: 0 },
      source: 'Ppm',
      time: { unTimeMicros: 0 }
    }
  });

  var TextView = Backbone.View.extend({
    initialize: function (options) {
      this.accessor = options.accessor;
      this.model.on('change', this.render, this);
      this.render();
    },
    render: function () {
      var val = this.accessor(this.model.toJSON())
      this.$el.html(val);
    }
  });

  var UserInputSliderView = Backbone.View.extend({
    initialize: function (options) {
      this.axis = options.axis;
      this.model.on('change', this.render, this);
      this.$progbar = $('#ui-' + this.axis + '-progbar');
      this.$label   = $('#ui-' + this.axis + '-progbar-lbl');
      this.render();
    },
    render: function () {
      var val = this.model.toJSON()['ui'][this.axis];
      this.$label.html(val.toFixed(2));
      var percent = (val + 1) * 50;
      this.$progbar.css('width', percent.toString() + '%');
    }
  });

  var UserInputView = function (opts) {
    this.throttle = new UserInputSliderView({ model: opts.model, axis: 'throttle' });
    this.roll     = new UserInputSliderView({ model: opts.model, axis: 'roll' });
    this.pitch    = new UserInputSliderView({ model: opts.model, axis: 'pitch' });
    this.yaw      = new UserInputSliderView({ model: opts.model, axis: 'yaw'  });
    this.source   = new TextView({
      el: '#ui-source',
      model: opts.model,
      accessor: function (json) { return json['source'].toString() }
    });
    this.time     = new TextView({
      el: '#ui-time',
      model: opts.model,
      accessor: function (json) {
        return (json['time']['unTimeMicros'] / 1000000).toFixed(1) + ' sec';
      }
    });
  };

  window.userInput =
    new UserInput({});
  window.userInputView =
    new UserInputView({ model: userInput });

  window.userInputScheduler =
    new Scheduler({ period: false}, userInput);
  window.userInputSchedulerView =
    new SchedulerButtonView({ model: userInputScheduler, el: '#ui-sch-btn' });

});




