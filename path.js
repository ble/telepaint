goog.require('ble.gfx.Drawable');

goog.provide('ble.gfx.path.Painter');
goog.provide('ble.gfx.path.painterDefault');
goog.provide('ble.gfx.path.PainterPixel');
goog.provide('ble.gfx.path.PainterVirtual');

/**
 * @constructor
 * @implements{ble.gfx.Drawable}
 */
ble.gfx.path.Painter = function(lineWidth, strokeStyle, opt_fillStyle) {
  this.lineWidth = lineWidth;
  this.strokeStyle = strokeStyle;
  if(goog.isDef(opt_fillStyle)) {
    this.fillStyle = opt_fillStyle;
    this.filled = true;
  } else {
    this.filled = false;
  }
};

ble.gfx.path.Painter.prototype.drawTo = goog.abstractMethod;

/**
 * @constructor
 * @private
 * @param {number} lineWidth
 * @param {string|CanvasGradient} strokeStyle
 * @param {string|CanvasGradient=} opt_fillStyle
 * @extends{ble.gfx.path.Painter}
 */
ble.gfx.path.PainterPixel = function(lineWidth, strokeStyle, opt_fillStyle) {
  ble.gfx.path.Painter.call(this, lineWidth, strokeStyle, opt_fillStyle);
};
goog.inherits(ble.gfx.path.PainterPixel, ble.gfx.path.Painter);

ble.gfx.path.PainterPixel.prototype.drawTo = function(ctx) {
  ctx.save();
  if(this.filled) {
    ctx.fillStyle = this.fillStyle;
    ctx.fill();
  } 
  ctx.lineWidth *= this.lineWidth;
  ctx.stroke();
  ctx.restore();
};

ble.gfx.path.PainterPixel.pool = {};
ble.gfx.path.PainterPixel.get = function(lineWidth, strokeStyle, opt_fillStyle) {
  var key = [lineWidth, strokeStyle, opt_fillStyle];
  var pool = ble.gfx.path.PainterPixel.pool;
  if(!(key in pool)) {
    pool[key] = new ble.gfx.path.PainterPixel(lineWidth, strokeStyle, opt_fillStyle);
  }
  return pool[key];
};

ble.gfx.path.painterDefault = new ble.gfx.path.PainterPixel(1, "#000000");

/**
 * @constructor
 * @extends{ble.gfx.path.Painter}
 */
ble.gfx.path.PainterVirtual = function(lineWidth, strokeStyle, opt_fillStyle) {
  ble.gfx.path.Painter.call(this, lineWidth, strokeStyle, opt_fillStyle);
};
goog.inherits(ble.gfx.path.PainterVirtual, ble.gfx.path.Painter);

ble.gfx.path.PainterVirtual.prototype.drawTo = function(ctx) {
  ctx.save();
  if(this.filled) {
    ctx.fillStyle = this.fillStyle;
    ctx.fill();
  } 
  ctx.lineWidth = this.lineWidth;
  ctx.stroke();
  ctx.restore(); 
};

