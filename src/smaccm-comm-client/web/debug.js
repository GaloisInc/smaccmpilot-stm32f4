window.ConsoleView = Backbone.View.extend({
  initialize: function (options) {
    this.model.on('change', this.render, this);
    this.options = options;
    this.render();
  },
  render: function () {
    var val = this.model.toJSON();
    if (this.options['selector']) {
      val = val[this.options['selector']];
    }
    console.log(val);
  }
});

window.JSONView = Backbone.View.extend({
  initialize: function (options) {
    this.model.on('change', this.render, this);
    this.options = options;
    this.render();
  },
  render: function () {
    var val = this.model.toJSON();
    if (this.options['selector']) {
      val = val[this.options['selector']];
    }
    this.$el.text(JSON.stringify(val));
  }
});
