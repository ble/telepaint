goog.provide('ble._2d');
goog.provide('ble._2d.DrawSurface');
goog.provide('ble._2d.Drawable');
goog.provide('ble._2d.TimeDrawable');

/**
 * @interface
 */
ble._2d.DrawSurface = function() {};
ble._2d.DrawSurface.prototype.withContext = function(action) {};

/**
 * @interface
 */
ble._2d.Drawable = function() {};
ble._2d.Drawable.prototype.drawTo = function(context) {};


/**
 * @interface
 */
ble._2d.TimeDrawable = function() {};

/**
 * @param {number} time
 * @param {CanvasRenderingContext2D} context
 */
ble._2d.TimeDrawable.prototype.drawPartialTo = function(time, context) {};

/**
 * @param {CanvasRenderingContext2D} context
 */
ble._2d.TimeDrawable.prototype.drawCompleteTo = function(context) {};

ble._2d.pathCoords = function(ctx, coords) {
  ctx.beginPath();
  if(coords.length == 0)
    return;
  ctx.moveTo(coords[0], coords[1]);
  for(var i = 1; i < coords.length / 2; i++)
    ctx.lineTo(coords[2*i], coords[2*i+1]);
}

ble._2d.pathCoordsWithin = function(ctx, coords, start, last) {
  ctx.beginPath();
  if(coords.length == 0)
    return;
  ctx.moveTo(coords[2*start], coords[2*start+1]);
  for(var i = start+1; i <= last; i++) {
    ctx.lineTo(coords[2*i], coords[2*i+1]);
  }
}
