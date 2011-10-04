goog.require('ble.gfx');
goog.require('ble.gfx.TimeDrawable');

goog.provide('ble.gfx.DrawPart');
goog.provide('ble.gfx.StrokeReplay'); 
goog.provide('ble.gfx.PolylineReplay');


/**
 * @interface
 * @extends {ble.gfx.TimeDrawable}
 */
ble.gfx.DrawPart = function() {};

/**
 * @return {number}
 */
ble.gfx.DrawPart.prototype.startTime = function() {};
/**
 * @return {number}
 */
ble.gfx.DrawPart.prototype.endTime = function() {};
/**
 * @param {number} millis
 * @return {ble.gfx.DrawPart}
 */
ble.gfx.DrawPart.prototype.withStartTime = function(millis) {}; 

/**
 * @constructor
 * @param {Array.<number>} coordinates
 * @param {Array.<number>} times
 * @param {number=} opt_lineWidth
 * @param {string|CanvasGradient=} opt_strokeStyle
 * @implements{ble.gfx.DrawPart}
 */
ble.gfx.StrokeReplay = function(coordinates, times, opt_lineWidth, opt_strokeStyle) {
  this.coordinates = coordinates;
  this.times = times;
  if(goog.isDef(opt_lineWidth)) {
    this.lineWidth = opt_lineWidth;
    this.definedWidth = true;
  }
  if(goog.isDef(opt_strokeStyle)) {
    this.strokeStyle = opt_strokeStyle;
    this.definedStyle = true;
  }
};

ble.gfx.StrokeReplay.prototype.toJSON = function() {
  var obj = ({
    '_tag': this._tag,
    'coordinates': this.coordinates,
    'times': this.times});
  if(this.lineWidth != ble.gfx.StrokeReplay.prototype.lineWidth) {
    obj['lineWidth'] = this.lineWidth;
  }
  if(this.strokeStyle != ble.gfx.StrokeReplay.prototype.strokeStyle) {
    obj['strokeStyle'] = this.strokeStyle;
  }
  return obj;
};

ble.gfx.StrokeReplay.bless = function(obj) {
  var tag = obj['_tag'];
  if(tag != ble.gfx.StrokeReplay.prototype._tag) return null;
  var c = obj['coordinates'];
  var t = obj['times'];
  var l = obj['lineWidth'];
  var s = obj['strokeStyle'];
  return new ble.gfx.StrokeReplay(c, t, l, s);
};


ble.gfx.StrokeReplay.prototype.startTime = function() {
  return this.times[0];
};

ble.gfx.StrokeReplay.prototype.endTime = function() {
  return this.times[this.times.length - 1];
};

/**
 * @param {ble.mocap.Capture} mocap
 * @param {number=} opt_lineWidth
 * @param {string|CanvasGradient=} opt_strokeStyle
 * @return {ble.gfx.StrokeReplay}
 */
ble.gfx.StrokeReplay.fromMocap = function(mocap, opt_lineWidth, opt_strokeStyle) {
  var coords = mocap.coordinates.slice();
  var times = mocap.times.slice();
  for(var i = 0; i < times.length; i++)
    times[i] += mocap.startTime;
  return new ble.gfx.StrokeReplay(coords, times, opt_lineWidth, opt_strokeStyle);
};

ble.gfx.StrokeReplay.prototype._tag = "ble.gfx.StrokeReplay";
ble.gfx.StrokeReplay.prototype.lineWidth = 1;
ble.gfx.StrokeReplay.prototype.strokeStyle = "#000000";

ble.gfx.StrokeReplay.prototype.drawPartialTo = function(time, ctx) {
  if(time >= this.endTime())
    this.drawCompleteTo(ctx);
  else {
    var indexEnd = Math.floor(ble.util.binarySearch(this.times, time));
    ble.gfx.pathPixCoordsWithin(ctx, this.coordinates, 0, indexEnd); 
    ctx.lineWidth = this.lineWidth;
    ctx.strokeStyle = this.strokeStyle;
    ctx.stroke();
  }
};

ble.gfx.StrokeReplay.prototype.drawCompleteTo = function(ctx) {
  ble.gfx.pathPixCoords(ctx, this.coordinates);
  ctx.lineWidth = this.lineWidth;
  ctx.strokeStyle = this.strokeStyle;
  ctx.stroke();
};

ble.gfx.StrokeReplay.prototype.withStartTime = function(newStart) {
  var delta = newStart - this.startTime();
  var newTimes = this.times.slice();
  for(var i = 0; i < newTimes.length; i++) {
    newTimes[i] += delta;
  }
  if(goog.isDef(this.definedWidth)) {
    if(goog.isDef(this.definedStyle))
      return new ble.gfx.StrokeReplay(this.coordinates, newTimes, this.lineWidth, this.strokeStyle);
    else
      return new ble.gfx.StrokeReplay(this.coordinates, newTimes, this.lineWidth);
  } else {
    return new ble.gfx.StrokeReplay(this.coordinates, newTimes);
  }
};

/**
 * @constructor
 * @param {Array.<number>} coordinates
 * @param {Array.<number>} times
 * @param {Array.<number>} controls
 * @param {number=} opt_lineWidth
 * @param {string|CanvasGradient=} opt_strokeStyle
 * @param {string|CanvasGradient=} opt_fillStyle
 */
ble.gfx.PolylineReplay = function(coordinates, times, controls, opt_lineWidth, opt_strokeStyle, opt_fillStyle) {
  this.coordinates = coordinates;
  this.times = times;
  this.controls = controls;
  if(goog.isDef(opt_lineWidth)) {
    this.lineWidth = opt_lineWidth;
    this.definedWidth = true;
  }
  if(goog.isDef(opt_strokeStyle)) {
    this.strokeStyle = opt_strokeStyle;
    this.definedStroke = true;
  }
  if(goog.isDef(opt_fillStyle)) {
    this.fillStyle = opt_fillStyle;
    this.filled = true;
  } else {
    this.filled = false;
  }
};

ble.gfx.PolylineReplay.prototype.toJSON = function() {
  var obj = ({
    '_tag': this._tag,
    'coordinates': this.coordinates,
    'times': this.times,
    'controls': this.controls});
  if(this.definedWidth) {
    obj['lineWidth'] = this.lineWidth;
  }
  if(this.definedStroke) {
    obj['strokeStyle'] = this.strokeStyle;
  }
  if(this.filled) {
    obj['fillStyle'] = this.fillStyle;
  }
  return obj;
};

ble.gfx.PolylineReplay.bless = function(obj) {
  var tag = obj['_tag'];
  if(tag != ble.gfx.PolylineReplay.prototype._tag) return null;
  var c = obj['coordinates'];
  var t = obj['times'];
  var cs = obj['controls'];
  var l = obj['lineWidth'];
  var s = obj['strokeStyle'];
  var f = obj['fillStyle'];
  return new ble.gfx.PolylineReplay(c, t, cs, l, s, f);
};

ble.gfx.PolylineReplay.fromMocap = function(mocap, opt_lineWidth, opt_strokeStyle, opt_fillStyle) {
  var coords = mocap.coordinates.slice();
  var times = mocap.times.slice();
  var controls = mocap.controlTimeIndices.slice();
  for(var i = 0; i < times.length; i++)
    times[i] += mocap.startTime;

  return new ble.gfx.PolylineReplay(coords, times, controls, opt_lineWidth, opt_strokeStyle, opt_fillStyle);
};

ble.gfx.PolylineReplay.prototype._tag = "ble.gfx.PolylineReplay";
ble.gfx.PolylineReplay.prototype.lineWidth = 1;
ble.gfx.PolylineReplay.prototype.strokeStyle = "#000000";
ble.gfx.PolylineReplay.prototype.startTime = function() {

  return this.times[0];
};

ble.gfx.PolylineReplay.prototype.endTime = function() {
  return this.times[this.times.length - 1];
};

ble.gfx.PolylineReplay.prototype.withStartTime = function(newStart) {
  var delta = newStart - this.startTime();
  var newTimes = this.times.slice();
  for(var i = 0; i < newTimes.length; i++) {
    newTimes[i] += delta;
  }
  var fillStyle = this.filled ? this.fillStyle : undefined;
  var strokeStyle = this.definedStroke ? this.strokeStyle : undefined;
  var lineWidth = this.definedWidth ? this.lineWidth : undefined;
  return new ble.gfx.PolylineReplay(this.coordinates, newTimes, this.controls, lineWidth, strokeStyle, fillStyle); 
};


ble.gfx.PolylineReplay.prototype.drawPartialTo = function(time, ctx) {
  if(time >= this.endTime())
    this.drawCompleteTo(ctx);
  else {
    var indexEnd = Math.floor(ble.util.binarySearch(this.times, time));
    var coords = [];
    for(var ix = 0, control; (control = this.controls[ix]) <= indexEnd; ix++) {
      coords.push(this.coordinates[2*control], this.coordinates[2*control+1]);
    }
    control = this.controls[ix-1];
    if(indexEnd > control) {
      coords.push(this.coordinates[2*indexEnd], this.coordinates[2*indexEnd+1]);
    }
    ble.gfx.pathPixCoords(ctx, coords);
    ctx.lineWidth = this.lineWidth;
    ctx.strokeStyle = this.strokeStyle;
    ctx.stroke(); 
  }
};

ble.gfx.PolylineReplay.prototype.drawCompleteTo = function(ctx) {
  var coords = [];
  for(var ix = 0; ix < this.controls.length; ix++) {
    var control = this.controls[ix];
    coords.push(this.coordinates[2*control], this.coordinates[2*control+1]);
  }
  if(this.controls[this.controls.length - 1] < this.times.length-1)
    coords.push(this.coordinates[this.coordinates.length - 2], this.coordinates[this.coordinates.length - 1]);
  ble.gfx.pathPixCoords(ctx, coords);
  ctx.lineWidth = this.lineWidth;
  ctx.strokeStyle = this.strokeStyle;
  ctx.stroke();
};


