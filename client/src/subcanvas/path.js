goog.require('ble._2d.Drawable');

goog.provide('ble._2d.path.Painter');
goog.provide('ble._2d.path.PainterPixel');
goog.provide('ble._2d.path.PainterVirtual');
goog.provide('ble._2d.path.painterDefault');

/**
 * @constructor
 * @implements{ble._2d.Drawable}
 */
ble._2d.path.Painter = function(lineWidth, strokeStyle, opt_fillStyle) {
  this.lineWidth = lineWidth;
  this.strokeStyle = strokeStyle;
  if(goog.isDef(opt_fillStyle)) {
    this.fillStyle = opt_fillStyle;
    this.filled = true;
  } else {
    this.filled = false;
  }
};

ble._2d.path.Painter.prototype.draw = goog.abstractMethod;

/**
 * @constructor
 * @private
 * @param {number} lineWidth
 * @param {string|CanvasGradient} strokeStyle
 * @param {string|CanvasGradient=} opt_fillStyle
 * @extends{ble._2d.path.Painter}
 */
ble._2d.path.PainterPixel = function(lineWidth, strokeStyle, opt_fillStyle) {
  ble._2d.path.Painter.call(this, lineWidth, strokeStyle, opt_fillStyle);
};
goog.inherits(ble._2d.path.PainterPixel, ble._2d.path.Painter);

ble._2d.path.PainterPixel.prototype.draw = function(ctx) {
  ctx.save();
  if(this.filled) {
    ctx.fillStyle = this.fillStyle;
    ctx.fill();
  } 
  ctx.lineWidth *= this.lineWidth;
  ctx.strokeStyle = this.strokeStyle;
  ctx.stroke();
  ctx.restore();
};

ble._2d.path.PainterPixel.pool = {};
ble._2d.path.PainterPixel.get = function(lineWidth, strokeStyle, opt_fillStyle) {
  var key = [lineWidth, strokeStyle, opt_fillStyle];
  var pool = ble._2d.path.PainterPixel.pool;
  if(!(key in pool)) {
    pool[key] = new ble._2d.path.PainterPixel(lineWidth, strokeStyle, opt_fillStyle);
  }
  return pool[key];
};


/**
 * @constructor
 * @param {number} lineWidth
 * @param {string|CanvasGradient} strokeStyle
 * @param {string|CanvasGradient=} opt_fillStyle
 * @extends{ble._2d.path.Painter}
 */
ble._2d.path.PainterVirtual = function(lineWidth, strokeStyle, opt_fillStyle) {
  ble._2d.path.Painter.call(this, lineWidth, strokeStyle, opt_fillStyle);
};
goog.inherits(ble._2d.path.PainterVirtual, ble._2d.path.Painter);

ble._2d.path.PainterVirtual.prototype.draw = function(ctx) {
  ctx.save();
  if(this.filled) {
    ctx.fillStyle = this.fillStyle;
    ctx.fill();
  } 
  ctx.lineWidth = this.lineWidth;
  ctx.strokeStyle = this.strokeStyle;
  ctx.stroke();
  ctx.restore(); 
};

ble._2d.path.PainterVirtual.pool = {};
ble._2d.path.PainterVirtual.get = function(lineWidth, strokeStyle, opt_fillStyle) {
  var key = [lineWidth, strokeStyle, opt_fillStyle];
  var pool = ble._2d.path.PainterPixel.pool;
  if(!(key in pool)) {
    pool[key] = new ble._2d.path.PainterVirtual(lineWidth, strokeStyle, opt_fillStyle);
  }
  return pool[key];
};

ble._2d.path.painterDefault = new ble._2d.path.PainterVirtual(1, "#000000");
