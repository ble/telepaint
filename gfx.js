goog.provide('ble.gfx');
goog.provide('ble.gfx.DrawSurface');
goog.provide('ble.gfx.Drawable');
goog.provide('ble.gfx.TimeDrawable');

/**
 * @interface
 */
ble.gfx.DrawSurface = function() {};
ble.gfx.DrawSurface.prototype.withContext = function(action) {};

/**
 * @interface
 */
ble.gfx.Drawable = function() {};
ble.gfx.Drawable.prototype.drawTo = function(context) {};


/**
 * @interface
 */
ble.gfx.TimeDrawable = function() {};

/**
 * @param {number} time
 * @param {CanvasRenderingContext2D} context
 */
ble.gfx.TimeDrawable.prototype.drawPartialTo = function(time, context) {};

/**
 * @param {CanvasRenderingContext2D} context
 */
ble.gfx.TimeDrawable.prototype.drawCompleteTo = function(context) {};

ble.gfx.pathCoords = function(ctx, coords) {
  ctx.beginPath();
  if(coords.length == 0)
    return;
  ctx.moveTo(coords[0], coords[1]);
  for(var i = 1; i < coords.length / 2; i++)
    ctx.lineTo(coords[2*i], coords[2*i+1]);
}

ble.gfx.pathCoordsWithin = function(ctx, coords, start, last) {
  ctx.beginPath();
  if(coords.length == 0)
    return;
  ctx.moveTo(coords[2*start], coords[2*start+1]);
  for(var i = start+1; i <= last; i++) {
    ctx.lineTo(coords[2*i], coords[2*i+1]);
  }
}
