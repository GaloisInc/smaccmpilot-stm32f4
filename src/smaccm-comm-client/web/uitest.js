

$(function() {

  var ControlLaw = Backbone.Model.extend({
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
  var ControlLawRequest = ControlLaw.extend({
    urlRoot: '/controllable_vehicle_i/control_law_request',
  });

  var ControlLawView = Backbone.View.extend({
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


  var ControlLawRequestView = ControlLawView.extend({
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

  window.controlLaw = new ControlLaw({});
  window.controlLawView = new ControlLawView({
    model: controlLaw,
    el: '#control-law-view'
  });
  window.controlLawScheduler =
    new Scheduler({ period: false}, controlLaw);
  window.controlLawSchedulerView =
    new SchedulerButtonView({ model: controlLawScheduler, el: '#cl-sch-btn' });

  window.controlLawRequest = new ControlLawRequest({});
  window.controlLawRequestView = new ControlLawRequestView({
    model: controlLawRequest,
    el: '#control-law-request-view'
  });
  window.controlLawRequestScheduler =
    new Scheduler({ period: false}, controlLawRequest);
  window.controlLawRequestSchedulerView =
    new SchedulerButtonView({ model: controlLawRequestScheduler, el: '#clr-sch-btn' });
});




