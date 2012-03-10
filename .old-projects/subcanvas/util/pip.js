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
  ctx.fillStyle = "#ffffff";
  ctx.fillRect(this.outer.left, this.outer.top, this.outer.right - this.outer.left, this.outer.bottom - this.outer.top);
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
  //console.log([o.left, o.top, i.left, i.top, ratioWidth, ratioHeight]);
  ctx.lineWidth /= (ratioWidth + ratioHeight) / 2;
};

/**
 * @constructor
 * @param {ble._2d.DrawPart} decorated
 * @param {goog.math.Box} fixedSource
 * @param {function(number): goog.math.Box} destinationFn
 * @implements {ble._2d.DrawPart}
 */
ble._2d.MovingPip = function(
    decorated,
    fixedSource,
    destinationFn,
    startMove,
    endMove) {
  this.decorated = decorated;
  this.source = fixedSource;
  this.destFn = destinationFn;
  this.startMove = startMove;
  this.endMove = endMove;
};

ble._2d.MovingPip.prototype.start = function() {
  return this.decorated.start();
};

ble._2d.MovingPip.prototype.end = function() {
  return Math.max(this.endMove, this.decorated.end());
};

ble._2d.MovingPip.prototype.length = function() {
  return this.end() - this.start();
};

ble._2d.MovingPip.prototype.withStartAt = function(newStart) {
  var delta = newStart - this.start();
  return new ble._2d.MovingPip(
      this.decorated.withStartAt(newStart),
      this.source,
      this.destFn,
      //keep `this.StartMove - this.start()` constant
      this.startMove + delta,
      this.endMove + delta); 
};

ble._2d.MovingPip.prototype.withLength = function(newLength) {
  var scaleFactor = newLength / this.length();
  var newDecoratedLength = this.decorated.length() * scaleFactor;
  return new ble._2d.MovingPip(
      this.decorated.withLength(newDecoratedLength),
      this.source,
      this.destFn,
      this.start() + (this.startMove - this.start()) * scaleFactor,
      this.start() + (this.endMove - this.start()) * scaleFactor);
};

ble._2d.MovingPip.prototype.getBoxAt_ = function(time) {
  var normalizedTime = (time - this.startMove) / (this.endMove - this.startMove);
  normalizedTime = Math.max(0, Math.min(1, normalizedTime));
  return this.destFn(normalizedTime);
};

ble._2d.MovingPip.prototype.draw = function(ctx) {
  this.at(this.endMove).draw(ctx);
};

ble._2d.MovingPip.prototype.at = function(time) {
  var box = this.getBoxAt_(time);
  return new ble._2d.PipDecorator(this.decorated.at(time), this.source, box);
};
