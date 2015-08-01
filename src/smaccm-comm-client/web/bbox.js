window.BBoxView = Backbone.View.extend({
  tagName: "canvas",
  initialize: function () {
    this.el.width = 320;
    this.el.height = 200;
    this.ctx = this.el.getContext("2d");
    this.listenTo(this.model, 'change', this.render);
    this.render();
  },
  render: function () {
    this.ctx.clearRect(0, 0, this.el.width, this.el.height);
    if (this.model.get('valid')) {
      var l = this.model.get('bbox_l'),
          r = this.model.get('bbox_r'),
          t = this.model.get('bbox_t'),
          b = this.model.get('bbox_b');
      this.ctx.fillStyle = '#FF6103';
      this.ctx.fillRect(l, t, r - l, b - t);
    }
    return this;
  }
});
