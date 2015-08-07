window.ControlLaw = Backbone.Model.extend({
  urlRoot: '/controllable_vehicle_i/control_law',
  defaults: {
    thr_mode: 'Direct',
    arming_mode: 'Safe',
    ui_mode: 'Ppm',
    yaw_mode: 'Rate'
  },
  valid : {
    thr_mode: [ 'Direct', 'Auto' ],
    arming_mode: [ 'Safe', 'Disarmed', 'Armed' ],
    ui_mode: [ 'Ppm', 'Gcs', 'Nav' ],
    yaw_mode: [ 'Rate', 'Heading' ]
  }
});
window.ControlLawRequest = ControlLaw.extend({
  urlRoot: '/controllable_vehicle_i/control_law_request',
});

window.ControlLawView = Backbone.View.extend({
  initialize: function () {
    this.$el.html(''
      + '<p>Arming Mode: '
      + this.button_group('arming_mode', this.model.valid.arming_mode)
      + '</p>'
      + '<p>Throttle Mode: '
      + this.button_group('thr_mode', this.model.valid.thr_mode)
      + '</p>'
      + '<p>UI Mode: '
      + this.button_group('ui_mode', this.model.valid.ui_mode)
      + '</p>'
      + '<p>Yaw Mode: '
      + this.button_group('yaw_mode', this.model.valid.yaw_mode)
      + '</p>'
      );
    this.model.on('change', this.render, this);
    this.render();
    if (this.post_init) {
      this.post_init();
    }
  },
  render: function () {
    var m = this.model.toJSON();
    var valid = this.model.valid;
    var self = this;
    _.map(valid, function (vs,k) {
      _.map(vs, function (v) {
        if (m[k] == v) {
          self.$('#btn-' + k + '-' + v)
              .removeClass('btn-default')
              .addClass('btn-success');
        } else {
          self.$('#btn-' + k + '-' + v)
              .removeClass('btn-success')
              .addClass('btn-default');
        }
      });
    });
  },
  button_group: function (group, labels) {
    var contents = _.map(labels, function(lbl) {
      return ('<button type="button" class="btn btn-default" id="btn-'
            + group + '-' + lbl +'">'
            + lbl + '</button>');
    });
    return ('<div class="btn-group btn-group-sm" role="group">' 
      + _.foldl(contents, function (a, b) { return a+b })
      + "</div>");
  }
});

window.ControlLawRequestView = ControlLawView.extend({
  post_init: function () {
    var m = this.model.toJSON();
    var valid = this.model.valid;
    var self = this;
    _.map(valid, function (vs,k) {
      _.map(vs, function (v) {
          self.$('#btn-' + k + '-' + v)
              .click(function () {
                self.model.set(k,v);
                self.model.save();
              });
      });
    });
  }
});

window.UserInput = Backbone.Model.extend({
  urlRoot: '/controllable_vehicle_i/user_input',
  defaults: {
    ui: { throttle: 0, roll: 0, pitch: 0, yaw: 0 },
    source: 'Ppm',
    time: { unTimeMicros: 0 }
  }
});

window.TextView = Backbone.View.extend({
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

window.UserInputSliderView = Backbone.View.extend({
  initialize: function (options) {
    this.accessor = options.accessor;
    this.model.on('change', this.render, this);
    this.$progbar = this.$('.progress-bar');
    this.$label   = this.$('.progress-bar-label');
    this.render();
  },
  render: function () {
    var val = this.accessor(this.model.toJSON()) || 0,
        valStr = val.toFixed(2),
        minVal = parseFloat(this.$progbar.attr('aria-valuemin')),
        maxVal = parseFloat(this.$progbar.attr('aria-valuemax')),
        percent = 100 * (val - minVal) / (maxVal - minVal);
    this.$progbar.attr('aria-valuenow', valStr);
    this.$label.text(valStr);
    this.$progbar.css('width', percent.toString() + '%');
  }
});

window.UserInputView = function (opts) {
  this.throttle = new UserInputSliderView({
    el: '#ui-throttle',
    model: opts.model,
    accessor: function (json) { return json.ui.throttle; },
  });
  this.roll     = new UserInputSliderView({
    el: '#ui-roll',
    model: opts.model,
    accessor: function (json) { return json.ui.roll; },
  });
  this.pitch    = new UserInputSliderView({
    el: '#ui-pitch',
    model: opts.model,
    accessor: function (json) { return json.ui.pitch; },
  });
  this.yaw      = new UserInputSliderView({
    el: '#ui-yaw',
    model: opts.model,
    accessor: function (json) { return json.ui.yaw; },
  });
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
