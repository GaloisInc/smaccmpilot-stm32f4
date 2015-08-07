window.Scheduler = Backbone.Model.extend({
  defaults: {
    period: false
  },
  initialize: function (attrs, models) {
    this.models = [].concat(models); // creates list from single object or list of objects
    this.interval = null;
    this.on('change', this.update, this);
    this.trigger('change', this);
  },
  update: function () {
    var attrs = this.toJSON();
    var self = this;
    if (this.interval) {
      clearInterval(this.interval);
    }
    if (attrs.period > 0) {
      this.interval = setInterval(function () {
        // Build up a chain of callbacks to fetch each model after the
        // previous one completes, because we don't seem to reliably
        // get replies when we have multiple requests pending.
        var chain = self.models.reduceRight(function (k, m) {
          return function () {
            m.fetch().done(k);
          };
        }, z);
        // The end of the chain does nothing, but we could invoke
        // setTimeout here if we didn't want the setInterval policy.
        function z() {}
        // Start the callback chain running.
        chain();
      }, attrs.period);
    } else {
      this.interval = null;
    }
  }
});

window.SchedulerButtonView = Backbone.View.extend({
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
