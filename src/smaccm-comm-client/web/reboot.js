window.RebootButtonView = Backbone.View.extend({
  tagName: 'button',
  attributes: { type: 'button' },
  className: 'btn btn-default',
  events: {
    'click' : 'click'
  },
  initialize: function (options) {
    this.$el.text('Reboot');
  },
  click: function () {
    this.model.set('magic', []);
    this.model.save();
  }
});
