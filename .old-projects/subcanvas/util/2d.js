goog.require('goog.math.Box');

goog.provide('ble._2d');
goog.provide('ble._2d.DrawSurface');
goog.provide('ble._2d.Drawable');
goog.provide('ble._2d.DrawPart');

/**
 * @interface
 */
ble._2d.DrawSurface = function() {};

/**
 * @param {function(CanvasRenderingContext2D)} action
 */
ble._2d.DrawSurface.prototype.withContext = function(action) {};

/**
 * @interface
 */
ble._2d.Drawable = function() {};

/**
 * @param {CanvasRenderingContext2D} context
 */
ble._2d.Drawable.prototype.draw = function(context) {};

/**
 * @interface
 * @extends {ble.interval.AdjustableInterval}
 * @extends {ble._2d.Drawable}
 */
ble._2d.DrawPart = function() {};

//specialize the return types of the AdjustableInterval types...
/**
 * @override
 * @param {number} newStart
 * @return {ble._2d.DrawPart}
 */
ble._2d.DrawPart.prototype.withStartAt = function(newStart) {};

/**
 * @override
 * @param {number} newLength
 * @return {ble._2d.DrawPart}
 */
ble._2d.DrawPart.prototype.withLength = function(newLength) {};


/**
 * @param {number} time
 * @return {ble._2d.Drawable}
 */
ble._2d.DrawPart.prototype.at = function(time) {};


/**
 * @constructor
 * @implements {ble._2d.Drawable}
 */
ble._2d.Nothing = function() {}; 
ble._2d.Nothing.prototype.draw = function(context) {};

/**
 * @type {ble._2d.Drawable}
 */
ble._2d.nothing = new ble._2d.Nothing();

ble._2d.pathCoords = function(ctx, coords) {
  ctx.beginPath();
  if(coords.length == 0)
    return;
  ctx.moveTo(coords[0], coords[1]);
  for(var i = 1; i < coords.length / 2; i++)
    ctx.lineTo(coords[2*i], coords[2*i+1]);
};

ble._2d.pathCoordsWithin = function(ctx, coords, start, last) {
  ctx.beginPath();
  if(coords.length == 0)
    return;
  ctx.moveTo(coords[2*start], coords[2*start+1]);
  for(var i = start+1; i <= last; i++) {
    ctx.lineTo(coords[2*i], coords[2*i+1]);
  }
};

