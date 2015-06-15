
$(function() {

  window.Scheduler = Backbone.Model.extend({
    defaults: {
      period: false
    },
    initialize: function (attrs, models) {
      this.models = [].concat(models); // creates list from single object or list of objects
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
        this.interval = setInterval(function () {
          _.each(self.models, function (m) { m.fetch(); });
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
});
