goog.require('goog.math.Box');
goog.require('ble._2d.Drawable');
goog.require('ble._2d.DrawPart');

goog.provide('ble._2d.PipDecorator');
goog.provide('ble._2d.MovingPip');
/**
 * @constructor
 * @param {ble._2d.Drawable} decorated
 * @param {goog.math.Box} innerSpace
 * @param {goog.math.Box} outerSpace
 * @implements {ble._2d.Drawable}
 */
ble._2d.PipDecorator = function(decorated, innerSpace, outerSpace) {
  this.decorated = decorated;
  this.inner = innerSpace;
  this.outer = outerSpace;
};

/**
 * @override
 */
ble._2d.PipDecorator.prototype.draw = function(ctx) {
  ctx.save();
  this.setup_(ctx);
  this.decorated.draw(ctx);
  ctx.restore();
};

/**
 * @private
 * @param {CanvasRenderingContext2D} ctx
 */
//It is assumed that the canvas state is saved before this is called and
//restored after drawing is completed.
ble._2d.PipDecorator.prototype.setup_ = function(ctx) {
  this.clip_(ctx);
  this.transform_(ctx);
};

/**
 * @private
 */
ble._2d.PipDecorator.prototype.clip_ = function(ctx) {
  var o = this.outer;
  ctx.beginPath();
  ctx.moveTo(o.left, o.top);
  ctx.lineTo(o.right, o.top);
  ctx.lineTo(o.right, o.bottom);
  ctx.lineTo(o.left, o.bottom);
  ctx.closePath();
  ctx.stroke();
  ctx.clip();
};

/**
 * @private
 */
ble._2d.PipDecorator.prototype.transform_ = function(ctx) {
  var o = this.outer;
  var i = this.inner;
  var ratioWidth = (o.right - o.left) / (i.right - i.left);
  var ratioHeight = (o.bottom - o.top) / (i.bottom - i.top);
  ctx.translate(o.left, o.top);
  ctx.scale(ratioWidth, ratioHeight);
  ctx.translate(-i.left, -i.top);
  ctx.lineWidth /= (ratioWidth + ratioHeight) / 2;
};

///**
// * @constructor
// * @param {ble._2d.DrawPart} decorated
// * @param {goog.math.Box} fixedSource
// * @param {function(number): goog.math.Box} destinationFn
// * @implements {ble._2d.DrawPart}
// */
//ble._2d.MovingPip = function(decorated, fixedSource, destinationFn) {
//
//};
