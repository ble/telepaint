goog.require('ble._2d');
goog.require('ble._2d.path.painterDefault');

goog.provide('ble._2d.Replay');
goog.provide('ble._2d.StrokeReplay'); 
goog.provide('ble._2d.PolylineReplay');
goog.provide('ble._2d.EraseReplay');


/**
 * @constructor
 * @param {number} startTime
 * @param {Array.<number>} coordinates
 * @param {Array.<number>} times
 */
ble._2d.Replay = function(startTime, coordinates, times) {
  this.startTime = startTime;
  this.coordinates = coordinates.slice();
  this.times = times.slice();
};

ble._2d.Replay.prototype.start = function() {
  return this.startTime;
};

ble._2d.Replay.prototype.end = function() {
  return this.startTime + this.times[this.times.length - 1];
};

ble._2d.Replay.prototype.length = function() {
  return this.end() - this.start();
};

ble._2d.Replay.prototype.withStartAt = function(newStart) {
  return new ble._2d.Replay(newStart, this.coordinates, this.times);
};

ble._2d.Replay.prototype.withLength = function(newLength) {
  var scaleFactor = newLength / this.length();
  var newTimes = this.times.map(function(t) { return scaleFactor * t; });
  return new ble._2d.Replay(this.startTime, this.coordinates, newTimes);
};

ble._2d.Replay.prototype.draw = goog.abstractMethod;
ble._2d.Replay.prototype.at = goog.abstractMethod;
ble._2d.Replay.prototype.toJSON = goog.abstractMethod;

/**
 * @constructor
 * @param {Array.<number>} coordinates
 * @param {Array.<number>} times
 * @param {ble._2d.path.Painter=} opt_painter
 * @extends{ble._2d.Replay}
 */
ble._2d.StrokeReplay = function(coordinates, startTime, times, opt_painter) {
  ble._2d.Replay.call(this, startTime, coordinates, times);
  if(goog.isDef(opt_painter))
    this.painter = opt_painter;
};
goog.inherits(ble._2d.StrokeReplay, ble._2d.Replay);

/**
 * @protected
 */
ble._2d.StrokeReplay.prototype.isStyled_ = function() {
  return this.painter !== ble._2d.StrokeReplay.prototype.painter;
};

/**
 * @protected
 */
ble._2d.StrokeReplay.prototype.getPainter_ = function() {
  return this.isStyled_() ? this.painter : undefined;
};

ble._2d.StrokeReplay.prototype.withStartAt = function(newStart) {
  return new ble._2d.StrokeReplay(
    this.coordinates,
    newStart,
    this.times,
    this.getPainter_());
};

ble._2d.StrokeReplay.prototype.withLength = function(newLength) {
  var scaleFactor = newLength / this.length();
  var newTimes = this.times.map(function(t) { return scaleFactor * t; });
  return new ble._2d.StrokeReplay(
    this.coordinates,
    this.startTime,
    newTimes,
    this.getPainter_());
};

ble._2d.StrokeReplay.prototype.draw = function(ctx) {
  ble._2d.pathCoords(ctx, this.coordinates);
  this.painter.draw(ctx);
};

ble._2d.StrokeReplay.prototype.at = function(time) {
  time -= this.start();
  if(time >= this.length())
    return this;
  if(time < 0)
    return ble._2d.nothing;

  var indexEnd = Math.floor(ble.util.binarySearch(this.times, time));
  var draw = goog.bind(this.drawIndex_, this, indexEnd);
  return {draw: draw};
};

/**
 * @private
 */
ble._2d.StrokeReplay.prototype.drawIndex_ = function(index, ctx) {
  ble._2d.pathCoordsWithin(ctx, this.coordinates, 0, index);
  this.painter.draw(ctx);
};

ble._2d.StrokeReplay.prototype.toJSON = function() {
  var obj = ({
    '_tag': this._tag,
    'coordinates': this.coordinates,
    'startTime': this.startTime,
    'times': this.times});
  var p = this.painter;
  if(p !== ble._2d.StrokeReplay.defaultPainter) {
    obj['lineWidth'] = p.lineWidth;
    obj['strokeStyle'] = p.strokeStyle;
    if(p.filled)
      obj['fillStyle'] = p.fillStyle;
  }
  return obj;
};

ble._2d.StrokeReplay.defaultPainter = ble._2d.path.painterDefault;
ble._2d.StrokeReplay.prototype.painter = ble._2d.StrokeReplay.defaultPainter;
ble._2d.StrokeReplay.prototype._tag = "ble._2d.StrokeReplay";

ble._2d.StrokeReplay.prototype.bless = function(obj) {
  var tag = obj['_tag'];
  if(tag != ble._2d.StrokeReplay.prototype._tag) return null;
  var c = obj['coordinates'];
  var s = obj['startTime'];
  var t = obj['times'];
  var L = obj['lineWidth'];
  var S = obj['strokeStyle'];
  var F = obj['fillStyle'];
  var painter;
  if(goog.isDef(L))
    painter = ble._2d.path.PainterVirtual.get(L, S, F);
  return new ble._2d.StrokeReplay(c, s, t, painter);
};

/**
 * @param {ble.mocap.Capture} mocap
 * @param {ble._2d.path.Painter=} opt_painter
 * @return {ble._2d.StrokeReplay}
 */
ble._2d.StrokeReplay.fromMocap = function(mocap, opt_painter) {
  return new ble._2d.StrokeReplay(
    mocap.coordinates.slice(),
    mocap.startTime,
    mocap.times.slice(),
    opt_painter);
};

/**
 * @constructor
 * @param {Array.<number>} coordinates
 * @param {Array.<number>} times
 * @param {Array.<number>} controls
 * @param {ble._2d.path.Painter=} opt_painter
 * @extends {ble._2d.Replay}
 */
ble._2d.PolylineReplay = function(coordinates, startTime, times, controls, opt_painter) {
  ble._2d.Replay.call(this, startTime, coordinates, times);
  this.controls = controls;
  if(goog.isDef(opt_painter))
    this.painter = opt_painter;
};
goog.inherits(ble._2d.PolylineReplay, ble._2d.Replay);

/**
 * @protected
 */
ble._2d.PolylineReplay.prototype.isStyled_ = function() {
  return this.painter !== ble._2d.PolylineReplay.prototype.painter;
};

/**
 * @protected
 */
ble._2d.PolylineReplay.prototype.getPainter_ = function() {
  return this.isStyled_() ? this.painter : undefined;
};

ble._2d.PolylineReplay.prototype.withStartAt = function(newStart) {
  return new ble._2d.PolylineReplay(
    this.coordinates,
    newStart,
    this.times,
    this.controls,
    this.getPainter_());
};

ble._2d.PolylineReplay.prototype.withLength = function(newLength) {
  var scaleFactor = newLength / this.length();
  var newTimes = this.times.map(function(t) { return scaleFactor * t; });
  return new ble._2d.PolylineReplay(
    this.coordinates,
    this.startTime,
    newTimes,
    this.controls,
    this.getPainter_());
};

ble._2d.PolylineReplay.prototype.draw = function(ctx) {
  var coords = [];
  for(var ix = 0; ix < this.controls.length; ix++) {
    var control = this.controls[ix];
    coords.push(this.coordinates[2*control], this.coordinates[2*control+1]);
  }
  if(this.controls[this.controls.length - 1] < this.times.length-1)
    coords.push(
      this.coordinates[this.coordinates.length - 2],
      this.coordinates[this.coordinates.length - 1]);

  ble._2d.pathCoords(ctx, coords);
  this.painter.draw(ctx);
};

ble._2d.PolylineReplay.prototype.at = function(time) {
  time -= this.start();
  if(time >= this.length())
    return this;
  if(time < 0)
    return ble._2d.nothing; 

  var indexEnd = Math.floor(ble.util.binarySearch(this.times, time));
  var draw = goog.bind(this.drawIndex_, this, indexEnd);
  return {draw: draw};
};

/**
 * @private
 */
ble._2d.PolylineReplay.prototype.drawIndex_ = function(indexEnd, ctx) { 
  var coords = [];

  //get the coordinates of all control points up to the ending
  //index, inclusive.
  for(var ix = 0, control; (control = this.controls[ix]) <= indexEnd; ix++) {
    coords.push(this.coordinates[2*control], this.coordinates[2*control+1]);
  }

  //add the coordinates of a non-control point if the end index is greater
  //than the index of the last control point.
  control = this.controls[ix-1];
  if(indexEnd > control) {
    coords.push(this.coordinates[2*indexEnd], this.coordinates[2*indexEnd+1]);
  }
  ble._2d.pathCoords(ctx, coords);
  this.painter.draw(ctx);
};


ble._2d.PolylineReplay.defaultPainter = ble._2d.path.painterDefault;
ble._2d.PolylineReplay.prototype.painter = ble._2d.PolylineReplay.defaultPainter;
ble._2d.PolylineReplay.prototype._tag = "ble._2d.PolylineReplay";

ble._2d.PolylineReplay.prototype.toJSON = function() {
  var obj = ({
    '_tag': this._tag,
    'coordinates': this.coordinates,
    'startTime': this.startTime,
    'times': this.times,
    'controls': this.controls});
  var p = this.painter;
  if(p !== ble._2d.PolylineReplay.defaultPainter) {
    obj['lineWidth'] = p.lineWidth;
    obj['strokeStyle'] = p.strokeStyle;
    if(p.filled)
      obj['fillStyle'] = p.fillStyle;
  }
  return obj;
};

ble._2d.PolylineReplay.prototype.bless = function(obj) {
  var tag = obj['_tag'];
  if(tag != ble._2d.PolylineReplay.prototype._tag) return null;
  var c = obj['coordinates'];
  var s = obj['startTime'];
  var t = obj['times'];
  var cs = obj['controls'];
  var L = obj['lineWidth'];
  var S = obj['strokeStyle'];
  var F = obj['fillStyle'];
  var painter;
  if(goog.isDef(L))
    painter = ble._2d.path.PainterVirtual.get(L, S, F);
  return new ble._2d.PolylineReplay(c, s, t, cs, painter);
};

/**
 * @param {ble.mocap.Capture} mocap
 * @param {ble._2d.path.Painter=} opt_painter
 */
ble._2d.PolylineReplay.fromMocap = function(mocap, opt_painter) {
  var coords = mocap.coordinates.slice();
  var times = mocap.times.slice();
  var controls = mocap.controlTimeIndices.slice();
  return new ble._2d.PolylineReplay(
    mocap.coordinates.slice(),
    mocap.startTime,
    mocap.times.slice(),
    mocap.controlTimeIndices.slice(),
    opt_painter);
};

/**
 * @constructor
 * @param {Array.<number>} coordinates
 * @param {Array.<number>} times
 * @param {number=} opt_lineWidth
 * @extends{ble._2d.Replay}
 */
ble._2d.EraseReplay = function(coordinates, startTime, times, opt_lineWidth) {
  ble._2d.Replay.call(this, startTime, coordinates, times);
  if(goog.isDef(opt_lineWidth))
    this.lineWidth = opt_lineWidth;
};
goog.inherits(ble._2d.EraseReplay, ble._2d.Replay);

ble._2d.EraseReplay.prototype.withStartAt = function(newStart) {
  return new ble._2d.EraseReplay(
    this.coordinates,
    newStart,
    this.times,
    this.lineWidth);
};

ble._2d.EraseReplay.prototype.withLength = function(newLength) {
  var scaleFactor = newLength / this.length();
  var newTimes = this.times.map(function(t) { return scaleFactor * t; });
  return new ble._2d.EraseReplay(
    this.coordinates, 
    this.startTime,
    newTimes,
    this.lineWidth);
};

/**
 * @private
 */
ble._2d.EraseReplay.prototype.paint_ = function(ctx) {
  ctx.save();
  ctx.strokeStyle = '#000000';
  ctx.globalCompositeOperation = 'destination-out';
  ctx.lineWidth = this.lineWidth;
  ctx.stroke(); 
  ctx.restore(); 
};

ble._2d.EraseReplay.prototype.draw = function(ctx) {
  ble._2d.pathCoords(ctx, this.coordinates);
  this.paint_(ctx);
};

ble._2d.EraseReplay.prototype.at = function(time) {
  time -= this.start();
  if(time >= this.length())
    return this;
  if(time < 0)
    return ble._2d.nothing; 

  var indexEnd = Math.floor(ble.util.binarySearch(this.times, time));
  var draw = goog.bind(this.drawIndex_, this, indexEnd);
  return {draw: draw};
};

/**
 * @private
 */
ble._2d.EraseReplay.prototype.drawIndex_ = function(index, ctx) {
  ble._2d.pathCoordsWithin(ctx, this.coordinates, 0, index);
  this.paint_(ctx);
};

ble._2d.EraseReplay.prototype._tag = "ble._2d.EraseReplay";
ble._2d.EraseReplay.prototype.lineWidth = 15;
goog.exportProperty(ble._2d.EraseReplay.prototype, 'lineWidth', ble._2d.EraseReplay.prototype.lineWidth);

ble._2d.EraseReplay.prototype.toJSON = function() {
  var obj = ({
    '_tag': this._tag,
    'coordinates': this.coordinates,
    'startTime': this.startTime,
    'times': this.times});
  if(this.hasOwnProperty('lineWidth'))
    obj['lineWidth'] = this.lineWidth;
  return obj;
};

ble._2d.EraseReplay.prototype.bless = function(obj) {
  var tag = obj['_tag'];
  if(tag != ble._2d.EraseReplay.prototype._tag) return null;
  var c = obj['coordinates'];
  var s = obj['startTime'];
  var t = obj['times'];
  var l = obj['lineWidth'];
  return new ble._2d.EraseReplay(c, s, t, l);
};

/**
 * @param {ble.mocap.Capture} mocap
 * @param {*} painter
 * @return {ble._2d.EraseReplay}
 */
ble._2d.EraseReplay.fromMocap = function(mocap, painter) {
  return new ble._2d.EraseReplay(
      mocap.coordinates.slice(),
      mocap.startTime,
      mocap.times.slice(),
      painter.lineWidth);
};
