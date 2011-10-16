goog.require('ble.gfx');
goog.require('ble.gfx.TimeDrawable');
goog.require('ble.gfx.path.PainterPixel');
goog.require('ble.gfx.path.painterDefault');

goog.provide('ble.gfx.DrawPart');
goog.provide('ble.gfx.Replay');
goog.provide('ble.gfx.StrokeReplay'); 
goog.provide('ble.gfx.PolylineReplay');
goog.provide('ble.gfx.EraseReplay');


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
 * @implements {ble.gfx.DrawPart}
 */
ble.gfx.Replay = function(coordinates, times) {
  this.coordinates = coordinates.slice();
  this.times = times.slice();
};

ble.gfx.Replay.prototype.toJSON = goog.abstractMethod;
ble.gfx.Replay.prototype.drawCompleteTo = goog.abstractMethod;
ble.gfx.Replay.prototype.drawPartialTo = goog.abstractMethod;
ble.gfx.Replay.prototype.withStartTime = goog.abstractMethod;

ble.gfx.Replay.prototype.startTime = function() {
  return this.times[0];
};

ble.gfx.Replay.prototype.endTime = function() {
  return this.times[this.times.length - 1];
};

ble.gfx.Replay.prototype.shiftedStartTimes = function(newStart) {
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
 * @param {ble.gfx.path.Painter=} opt_painter
 * @extends{ble.gfx.Replay}
 */
ble.gfx.StrokeReplay = function(coordinates, times, opt_painter) {
  ble.gfx.Replay.call(this, coordinates, times);
  if(goog.isDef(opt_painter))
    this.painter = opt_painter;
};
goog.inherits(ble.gfx.StrokeReplay, ble.gfx.Replay);

ble.gfx.StrokeReplay.defaultPainter = ble.gfx.path.painterDefault;
ble.gfx.StrokeReplay.prototype.painter = ble.gfx.StrokeReplay.defaultPainter;
ble.gfx.StrokeReplay.prototype._tag = "ble.gfx.StrokeReplay";

ble.gfx.StrokeReplay.prototype.toJSON = function() {
  var obj = ({
    '_tag': this._tag,
    'coordinates': this.coordinates,
    'times': this.times});
  var p = this.painter;
  if(p !== ble.gfx.StrokeReplay.defaultPainter) {
    obj['lineWidth'] = p.lineWidth;
    obj['strokeStyle'] = p.strokeStyle;
    if(p.filled)
      obj['fillStyle'] = p.fillStyle;
  }
  return obj;
};

ble.gfx.StrokeReplay.prototype.bless = function(obj) {
  var tag = obj['_tag'];
  if(tag != ble.gfx.StrokeReplay.prototype._tag) return null;
  var c = obj['coordinates'];
  var t = obj['times'];
  var L = obj['lineWidth'];
  var S = obj['strokeStyle'];
  var F = obj['fillStyle'];
  var painter;
  if(goog.isDef(L))
    painter = ble.gfx.path.PainterVirtual.get(L, S, F);
  return new ble.gfx.StrokeReplay(c, t, painter);
};

/**
 * @param {ble.mocap.Capture} mocap
 * @param {ble.gfx.path.Painter=} opt_painter
 * @return {ble.gfx.StrokeReplay}
 */
ble.gfx.StrokeReplay.fromMocap = function(mocap, opt_painter) {
  var coords = mocap.coordinates.slice();
  var times = mocap.times.slice();
  for(var i = 0; i < times.length; i++)
    times[i] += mocap.startTime;
  return new ble.gfx.StrokeReplay(coords, times, opt_painter);
};

ble.gfx.StrokeReplay.prototype.withStartTime = function(newStart) {
  var newTimes = this.shiftedStartTimes(newStart);
  var ownPainter = this.painter;
  if(ownPainter === ble.gfx.StrokeReplay.prototype.painter)
    ownPainter = undefined;
  return new ble.gfx.StrokeReplay(this.coordinates, newTimes, ownPainter);
}; 


ble.gfx.StrokeReplay.prototype.drawPartialTo = function(time, ctx) {
  if(time >= this.endTime())
    this.drawCompleteTo(ctx);
  else {
    var indexEnd = Math.floor(ble.util.binarySearch(this.times, time));
    ble.gfx.pathCoordsWithin(ctx, this.coordinates, 0, indexEnd); 
    this.painter.drawTo(ctx);
  }
};

ble.gfx.StrokeReplay.prototype.drawCompleteTo = function(ctx) {
  ble.gfx.pathCoords(ctx, this.coordinates);
  this.painter.drawTo(ctx);
};

/**
 * @constructor
 * @param {Array.<number>} coordinates
 * @param {Array.<number>} times
 * @param {Array.<number>} controls
 * @param {ble.gfx.path.Painter=} opt_painter
 * @extends {ble.gfx.Replay}
 */
ble.gfx.PolylineReplay = function(coordinates, times, controls, opt_painter) {
  ble.gfx.Replay.call(this, coordinates, times);
  this.controls = controls;
  if(goog.isDef(opt_painter))
    this.painter = opt_painter;
};
goog.inherits(ble.gfx.PolylineReplay, ble.gfx.Replay);

ble.gfx.PolylineReplay.defaultPainter = ble.gfx.path.painterDefault;
ble.gfx.PolylineReplay.prototype.painter = ble.gfx.PolylineReplay.defaultPainter;
ble.gfx.PolylineReplay.prototype._tag = "ble.gfx.PolylineReplay";

ble.gfx.PolylineReplay.prototype.toJSON = function() {
  var obj = ({
    '_tag': this._tag,
    'coordinates': this.coordinates,
    'times': this.times,
    'controls': this.controls});
  var p = this.painter;
  if(p !== ble.gfx.PolylineReplay.defaultPainter) {
    obj['lineWidth'] = p.lineWidth;
    obj['strokeStyle'] = p.strokeStyle;
    if(p.filled)
      obj['fillStyle'] = p.fillStyle;
  }
  return obj;
};

ble.gfx.PolylineReplay.prototype.bless = function(obj) {
  var tag = obj['_tag'];
  if(tag != ble.gfx.PolylineReplay.prototype._tag) return null;
  var c = obj['coordinates'];
  var t = obj['times'];
  var cs = obj['controls'];
  var L = obj['lineWidth'];
  var S = obj['strokeStyle'];
  var F = obj['fillStyle'];
  var painter;
  if(goog.isDef(L))
    painter = ble.gfx.path.PainterVirtual.get(L, S, F);
  return new ble.gfx.PolylineReplay(c, t, cs, painter);
};
/**
 * @param {ble.mocap.Capture} mocap
 * @param {ble.gfx.path.Painter=} opt_painter
 */
ble.gfx.PolylineReplay.fromMocap = function(mocap, opt_painter) {
  var coords = mocap.coordinates.slice();
  var times = mocap.times.slice();
  var controls = mocap.controlTimeIndices.slice();
  for(var i = 0; i < times.length; i++)
    times[i] += mocap.startTime;
  return new ble.gfx.PolylineReplay(coords, times, controls, opt_painter);
};

ble.gfx.PolylineReplay.prototype.withStartTime = function(newStart) {
  var newTimes = this.shiftedStartTimes(newStart);
  var ownPainter = this.painter;
  if(ownPainter === ble.gfx.PolylineReplay.prototype.painter)
    ownPainter = undefined; 
  return new ble.gfx.PolylineReplay(this.coordinates, newTimes, this.controls, ownPainter);
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
    ble.gfx.pathCoords(ctx, coords);
    this.painter.drawTo(ctx);
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
  ble.gfx.pathCoords(ctx, coords);
  this.painter.drawTo(ctx);
};

/**
 * @constructor
 * @param {Array.<number>} coordinates
 * @param {Array.<number>} times
 * @param {number=} opt_lineWidth
 * @extends{ble.gfx.Replay}
 */
ble.gfx.EraseReplay = function(coordinates, times, opt_lineWidth) {
  ble.gfx.Replay.call(this, coordinates, times);
  if(goog.isDef(opt_lineWidth))
    this.lineWidth = opt_lineWidth;
};
goog.inherits(ble.gfx.EraseReplay, ble.gfx.Replay);

ble.gfx.EraseReplay.prototype._tag = "ble.gfx.EraseReplay";
ble.gfx.EraseReplay.prototype.lineWidth = 15;
goog.exportProperty(ble.gfx.EraseReplay.prototype, 'lineWidth', ble.gfx.EraseReplay.prototype.lineWidth);

ble.gfx.EraseReplay.prototype.toJSON = function() {
  var obj = ({
    '_tag': this._tag,
    'coordinates': this.coordinates,
    'times': this.times});
  if(this.hasOwnProperty('lineWidth'))
    obj['lineWidth'] = this.lineWidth;
  return obj;
};

ble.gfx.EraseReplay.prototype.bless = function(obj) {
  var tag = obj['_tag'];
  if(tag != ble.gfx.EraseReplay.prototype._tag) return null;
  var c = obj['coordinates'];
  var t = obj['times'];
  var l = obj['lineWidth'];
  return new ble.gfx.EraseReplay(c, t, l);
};

/**
 * @param {ble.mocap.Capture} mocap
 * @param {*} ignored
 * @return {ble.gfx.EraseReplay}
 */
ble.gfx.EraseReplay.fromMocap = function(mocap, ignored) {
  var coords = mocap.coordinates.slice();
  var times = mocap.times.slice();
  for(var i = 0; i < times.length; i++)
    times[i] += mocap.startTime;
  return new ble.gfx.EraseReplay(coords, times);
};

ble.gfx.EraseReplay.prototype.withStartTime = function(newStart) {
  var newTimes = this.shiftedStartTimes(newStart);
  return new ble.gfx.EraseReplay(this.coordinates, newTimes);
}; 


ble.gfx.EraseReplay.prototype.drawPartialTo = function(time, ctx) {
  if(time >= this.endTime())
    this.drawCompleteTo(ctx);
  else {
    var indexEnd = Math.floor(ble.util.binarySearch(this.times, time));
    ctx.save();
    ble.gfx.pathCoordsWithin(ctx, this.coordinates, 0, indexEnd); 
    ctx.strokeStyle = 'rgba(0, 0, 0, 0.0)';
    ctx.globalCompositeOperation = 'copy';
    ctx.lineWidth = this.lineWidth;
    ctx.stroke(); 
    ctx.restore();
  }
};

ble.gfx.EraseReplay.prototype.drawCompleteTo = function(ctx) {
  ctx.save();
  ble.gfx.pathCoords(ctx, this.coordinates);
  ctx.strokeStyle = 'rgba(0, 0, 0, 0.0)';
  ctx.globalCompositeOperation = 'copy';
  ctx.lineWidth = this.lineWidth;
  ctx.stroke(); 
  ctx.restore();
};

