window.ButtonGroupView = Backbone.View.extend({
  tagName: "p",
  initialize: function (options) {
    this.field = options.field;
    var self = this,
        group = $('<span class="btn-group btn-group-sm" role="group" />');
    _.each(options.values, function (v) {
      var btn = $('<button type="button" class="btn btn-default"></button>').text(v);
      if (options.writable)
        btn.click(function () {
          self.model.set(self.field, v);
          self.model.save();
        });
      else
        btn.prop('disabled', true);
      group.append(btn);
    });
    this.$el.text(options.label + ': ').append(group);
    this.listenTo(this.model, 'change', this.render);
    this.render();
  },
  render: function () {
    var value = this.model.get(this.field);
    this.$('button').each(function () {
      var $el = $(this);
      if ($el.text() == value)
        $el.removeClass('btn-default').addClass('btn-success');
      else
        $el.removeClass('btn-success').addClass('btn-default');
    });
  },
});

window.ControlLaw = Backbone.Model.extend({
  urlRoot: '/controllable_vehicle_i/control_law',
});
window.ControlLawRequest = ControlLaw.extend({
  urlRoot: '/controllable_vehicle_i/control_law_request',
});

window.BackboneLens = Backbone.Model.extend({
  constructor: function (attrs, options) {
    this.base = options.base;
    this.field = options.field;
    Backbone.Model.apply(this, arguments);
  },
  get: function (field) {
    var into = this.base.get(this.field);
    return (into || {})[field];
  },
  set: function (key, val, options) {
    var attrs;
    if (typeof key === 'object') {
      attrs = key;
      options = val;
    } else {
      (attrs = {})[key] = val;
    }

    var old = this.base.get(this.field);
    if (old)
      _.defaults(attrs, old);
    this.base.set(this.field, old, options);
  },
  on: function (event, callback, context) {
    // XXX this bare minimum definition will fail for many use cases
    this.base.on(event, callback, context);
  },
});

window.ControlLawView = function (options) {
  this.$el = $(options.el);
  var control_modes = new BackboneLens(null, { base: options.model, field: 'control_modes' });
  this.$el.append([
    new ButtonGroupView({
      label: 'Arming Mode',
      field: 'arming_mode',
      values: [ 'Safe', 'Armed' ],
      model: options.model,
      writable: options.writable,
    }).el,
    new ButtonGroupView({
      label: 'Throttle Mode',
      field: 'thr_mode',
      values: [ 'DirectUi', 'AltUi', 'AltSetpt' ],
      model: control_modes,
      writable: options.writable,
    }).el,
    new ButtonGroupView({
      label: 'UI Mode',
      field: 'ui_mode',
      values: [ 'Ppm', 'Gcs', 'Nav' ],
      model: control_modes,
      writable: options.writable,
    }).el,
    new ButtonGroupView({
      label: 'Yaw Mode',
      field: 'yaw_mode',
      values: [ 'Rate', 'Heading' ],
      model: control_modes,
      writable: options.writable,
    }).el,
  ]);
};

window.ControlLawRequestView = function (options) {
  options.writable = true;
  ControlLawView(options);
};

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
