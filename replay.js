goog.require('ble._2d');
goog.require('ble._2d.TimeDrawable');
goog.require('ble._2d.path.PainterPixel');
goog.require('ble._2d.path.painterDefault');

goog.provide('ble._2d.DrawPart');
goog.provide('ble._2d.Replay');
goog.provide('ble._2d.StrokeReplay'); 
goog.provide('ble._2d.PolylineReplay');
goog.provide('ble._2d.EraseReplay');


/**
 * @interface
 * @extends {ble._2d.TimeDrawable}
 */
ble._2d.DrawPart = function() {};

/**
 * @return {number}
 */
ble._2d.DrawPart.prototype.startTime = function() {};
/**
 * @return {number}
 */
ble._2d.DrawPart.prototype.endTime = function() {};
/**
 * @param {number} millis
 * @return {ble._2d.DrawPart}
 */
ble._2d.DrawPart.prototype.withStartTime = function(millis) {}; 


/**
 * @constructor
 * @param {Array.<number>} coordinates
 * @param {Array.<number>} times
 * @implements {ble._2d.DrawPart}
 */
ble._2d.Replay = function(coordinates, times) {
  this.coordinates = coordinates.slice();
  this.times = times.slice();
};

ble._2d.Replay.prototype.toJSON = goog.abstractMethod;
ble._2d.Replay.prototype.drawCompleteTo = goog.abstractMethod;
ble._2d.Replay.prototype.drawPartialTo = goog.abstractMethod;
ble._2d.Replay.prototype.withStartTime = goog.abstractMethod;

ble._2d.Replay.prototype.startTime = function() {
  return this.times[0];
};

ble._2d.Replay.prototype.endTime = function() {
  return this.times[this.times.length - 1];
};

ble._2d.Replay.prototype.shiftedStartTimes = function(newStart) {
  var delta = newStart - this.startTime();
  var newTimes = this.times.slice();
  for(var i = 0; i < newTimes.length; i++) {
    newTimes[i] += delta;
  }
  return newTimes;
}

/**
 * @constructor
 * @param {Array.<number>} coordinates
 * @param {Array.<number>} times
 * @param {ble._2d.path.Painter=} opt_painter
 * @extends{ble._2d.Replay}
 */
ble._2d.StrokeReplay = function(coordinates, times, opt_painter) {
  ble._2d.Replay.call(this, coordinates, times);
  if(goog.isDef(opt_painter))
    this.painter = opt_painter;
};
goog.inherits(ble._2d.StrokeReplay, ble._2d.Replay);

ble._2d.StrokeReplay.defaultPainter = ble._2d.path.painterDefault;
ble._2d.StrokeReplay.prototype.painter = ble._2d.StrokeReplay.defaultPainter;
ble._2d.StrokeReplay.prototype._tag = "ble._2d.StrokeReplay";

ble._2d.StrokeReplay.prototype.toJSON = function() {
  var obj = ({
    '_tag': this._tag,
    'coordinates': this.coordinates,
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

ble._2d.StrokeReplay.prototype.bless = function(obj) {
  var tag = obj['_tag'];
  if(tag != ble._2d.StrokeReplay.prototype._tag) return null;
  var c = obj['coordinates'];
  var t = obj['times'];
  var L = obj['lineWidth'];
  var S = obj['strokeStyle'];
  var F = obj['fillStyle'];
  var painter;
  if(goog.isDef(L))
    painter = ble._2d.path.PainterVirtual.get(L, S, F);
  return new ble._2d.StrokeReplay(c, t, painter);
};

/**
 * @param {ble.mocap.Capture} mocap
 * @param {ble._2d.path.Painter=} opt_painter
 * @return {ble._2d.StrokeReplay}
 */
ble._2d.StrokeReplay.fromMocap = function(mocap, opt_painter) {
  var coords = mocap.coordinates.slice();
  var times = mocap.times.slice();
  for(var i = 0; i < times.length; i++)
    times[i] += mocap.startTime;
  return new ble._2d.StrokeReplay(coords, times, opt_painter);
};

ble._2d.StrokeReplay.prototype.withStartTime = function(newStart) {
  var newTimes = this.shiftedStartTimes(newStart);
  var ownPainter = this.painter;
  if(ownPainter === ble._2d.StrokeReplay.prototype.painter)
    ownPainter = undefined;
  return new ble._2d.StrokeReplay(this.coordinates, newTimes, ownPainter);
}; 


ble._2d.StrokeReplay.prototype.drawPartialTo = function(time, ctx) {
  if(time >= this.endTime())
    this.drawCompleteTo(ctx);
  else {
    var indexEnd = Math.floor(ble.util.binarySearch(this.times, time));
    ble._2d.pathCoordsWithin(ctx, this.coordinates, 0, indexEnd); 
    this.painter.draw(ctx);
  }
};

ble._2d.StrokeReplay.prototype.drawCompleteTo = function(ctx) {
  ble._2d.pathCoords(ctx, this.coordinates);
  this.painter.draw(ctx);
};

/**
 * @constructor
 * @param {Array.<number>} coordinates
 * @param {Array.<number>} times
 * @param {Array.<number>} controls
 * @param {ble._2d.path.Painter=} opt_painter
 * @extends {ble._2d.Replay}
 */
ble._2d.PolylineReplay = function(coordinates, times, controls, opt_painter) {
  ble._2d.Replay.call(this, coordinates, times);
  this.controls = controls;
  if(goog.isDef(opt_painter))
    this.painter = opt_painter;
};
goog.inherits(ble._2d.PolylineReplay, ble._2d.Replay);

ble._2d.PolylineReplay.defaultPainter = ble._2d.path.painterDefault;
ble._2d.PolylineReplay.prototype.painter = ble._2d.PolylineReplay.defaultPainter;
ble._2d.PolylineReplay.prototype._tag = "ble._2d.PolylineReplay";

ble._2d.PolylineReplay.prototype.toJSON = function() {
  var obj = ({
    '_tag': this._tag,
    'coordinates': this.coordinates,
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
  var t = obj['times'];
  var cs = obj['controls'];
  var L = obj['lineWidth'];
  var S = obj['strokeStyle'];
  var F = obj['fillStyle'];
  var painter;
  if(goog.isDef(L))
    painter = ble._2d.path.PainterVirtual.get(L, S, F);
  return new ble._2d.PolylineReplay(c, t, cs, painter);
};
/**
 * @param {ble.mocap.Capture} mocap
 * @param {ble._2d.path.Painter=} opt_painter
 */
ble._2d.PolylineReplay.fromMocap = function(mocap, opt_painter) {
  var coords = mocap.coordinates.slice();
  var times = mocap.times.slice();
  var controls = mocap.controlTimeIndices.slice();
  for(var i = 0; i < times.length; i++)
    times[i] += mocap.startTime;
  return new ble._2d.PolylineReplay(coords, times, controls, opt_painter);
};

ble._2d.PolylineReplay.prototype.withStartTime = function(newStart) {
  var newTimes = this.shiftedStartTimes(newStart);
  var ownPainter = this.painter;
  if(ownPainter === ble._2d.PolylineReplay.prototype.painter)
    ownPainter = undefined; 
  return new ble._2d.PolylineReplay(this.coordinates, newTimes, this.controls, ownPainter);
};


ble._2d.PolylineReplay.prototype.drawPartialTo = function(time, ctx) {
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
    ble._2d.pathCoords(ctx, coords);
    this.painter.draw(ctx);
  }
};

ble._2d.PolylineReplay.prototype.drawCompleteTo = function(ctx) {
  var coords = [];
  for(var ix = 0; ix < this.controls.length; ix++) {
    var control = this.controls[ix];
    coords.push(this.coordinates[2*control], this.coordinates[2*control+1]);
  }
  if(this.controls[this.controls.length - 1] < this.times.length-1)
    coords.push(this.coordinates[this.coordinates.length - 2], this.coordinates[this.coordinates.length - 1]);
  ble._2d.pathCoords(ctx, coords);
  this.painter.draw(ctx);
};

/**
 * @constructor
 * @param {Array.<number>} coordinates
 * @param {Array.<number>} times
 * @param {number=} opt_lineWidth
 * @extends{ble._2d.Replay}
 */
ble._2d.EraseReplay = function(coordinates, times, opt_lineWidth) {
  ble._2d.Replay.call(this, coordinates, times);
  if(goog.isDef(opt_lineWidth))
    this.lineWidth = opt_lineWidth;
};
goog.inherits(ble._2d.EraseReplay, ble._2d.Replay);

ble._2d.EraseReplay.prototype._tag = "ble._2d.EraseReplay";
ble._2d.EraseReplay.prototype.lineWidth = 15;
goog.exportProperty(ble._2d.EraseReplay.prototype, 'lineWidth', ble._2d.EraseReplay.prototype.lineWidth);

ble._2d.EraseReplay.prototype.toJSON = function() {
  var obj = ({
    '_tag': this._tag,
    'coordinates': this.coordinates,
    'times': this.times});
  if(this.hasOwnProperty('lineWidth'))
    obj['lineWidth'] = this.lineWidth;
  return obj;
};

ble._2d.EraseReplay.prototype.bless = function(obj) {
  var tag = obj['_tag'];
  if(tag != ble._2d.EraseReplay.prototype._tag) return null;
  var c = obj['coordinates'];
  var t = obj['times'];
  var l = obj['lineWidth'];
  return new ble._2d.EraseReplay(c, t, l);
};

/**
 * @param {ble.mocap.Capture} mocap
 * @param {*} painter
 * @return {ble._2d.EraseReplay}
 */
ble._2d.EraseReplay.fromMocap = function(mocap, painter) {
  var coords = mocap.coordinates.slice();
  var times = mocap.times.slice();
  for(var i = 0; i < times.length; i++)
    times[i] += mocap.startTime;
  return new ble._2d.EraseReplay(coords, times, painter.lineWidth);
};

ble._2d.EraseReplay.prototype.withStartTime = function(newStart) {
  var newTimes = this.shiftedStartTimes(newStart);
  return new ble._2d.EraseReplay(this.coordinates, newTimes, this.lineWidth);
}; 


ble._2d.EraseReplay.prototype.drawPartialTo = function(time, ctx) {
  if(time >= this.endTime())
    this.drawCompleteTo(ctx);
  else {
    var indexEnd = Math.floor(ble.util.binarySearch(this.times, time));
    ctx.save();
    ble._2d.pathCoordsWithin(ctx, this.coordinates, 0, indexEnd); 
    ctx.strokeStyle = 'rgba(0, 0, 0, 1.0)';
    ctx.globalCompositeOperation = 'destination-out';
    ctx.lineWidth = this.lineWidth;
    ctx.stroke(); 
    ctx.restore();
  }
};

ble._2d.EraseReplay.prototype.drawCompleteTo = function(ctx) {
  ctx.save();
  ble._2d.pathCoords(ctx, this.coordinates);
  ctx.strokeStyle = 'rgba(0, 0, 0, 1.0)';
  ctx.globalCompositeOperation = 'destination-out';
  ctx.lineWidth = this.lineWidth;
  ctx.stroke(); 
  ctx.restore();
};

