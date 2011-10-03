goog.provide('ble.Scribble2');
goog.provide('ble.scribble.Painter');
goog.require('ble.scratch.Canvas');
goog.require('ble.mocap.Stroke');
goog.require('ble.gfx');
goog.require('ble.gfx.StrokeReplay');
goog.require('ble.gfx.TimeDrawable');
goog.require('goog.events');
goog.require('goog.events.EventType');

/**
 * @constructor
 * @param {Array.<ble.gfx.DrawPart>} items
 * @implements {ble.gfx.DrawPart}
 */
ble.scribble.Painter = function(items) {
  this.data = items.slice();
  this.paintReady = this.zeroReference(this.data);
  this.beingDrawn = null;
};

ble.scribble.Painter.prototype.startTime = function() {
  return this.paintReady.length > 0 ? this.paintReady[0].startTime()
                                    : 0;
};

ble.scribble.Painter.prototype.endTime = function() {
  return this.paintReady.length > 0 ? this.paintReady[this.paintReady.length - 1].endTime()
                                    : 0;
};

ble.scribble.Painter.prototype.withStartTime = function(newStart) {
  var result = new ble.scribble.Painter(this.data);
  var toAlter = result.paintReady;
  var lastEnd = newStart;
  for(var i = 0; i < toAlter.length; i++) {
    toAlter[i] = toAlter[i].withStartTime(lastEnd);
    lastEnd = toAlter[i].endTime();
  }
  return result;
};

ble.scribble.Painter.prototype.toJSON = function() {
  return this.data;
};

/**
 * @param {Array.<ble.gfx.DrawPart>} parts
 */
ble.scribble.Painter.prototype.zeroReference = function(parts) {
  var result = [];
  var lastEnd = 0;
  for(var i = 0; i < parts.length; i++) {
    result[i] = parts[i].withStartTime(lastEnd);
    lastEnd = result[i].endTime();
  };
  return result;
};

/**
 * @param {ble.gfx.DrawPart} current
 */
ble.scribble.Painter.prototype.setCurrent = function(current) {
  this.beingDrawn = current;
};

ble.scribble.Painter.prototype.recordCurrent = function() {
  var current = this.beingDrawn;
  if(!goog.isNull(current)) {
    this.data.push(current);
    var adjusted = current.withStartTime(this.endTime());
    this.paintReady.push(adjusted);
    this.beingDrawn = null; 
  }
};

ble.scribble.Painter.prototype.drawCompleteTo = function(ctx) {
  if(!goog.isNull(this.beingDrawn)) {
    this.beingDrawn.drawCompleteTo(ctx);
  }
  for(var i = 0; i < this.paintReady.length; i++)
    this.paintReady[i].drawCompleteTo(ctx); 
};

ble.scribble.Painter.prototype.drawPartialTo = function(time, ctx) {
  if(!goog.isNull(this.beingDrawn)) {
    this.beingDrawn.drawCompleteTo(ctx);
  }
  for(var i = 0; i < this.paintReady.length; i++) {
    var item = this.paintReady[i];
    if(item.startTime() > time)
      break;
    item.drawPartialTo(time, ctx);
  } 
};

/**
 * @constructor
 * @param {number} width
 * @param {number} height
 * @param {ble.scribble.Painter=} opt_painter
 * @extends {ble.scratch.Canvas}
 */
ble.Scribble2 = function(width, height, opt_painter) {
  ble.scratch.Canvas.call(this, width, height);
  if(goog.isDef(opt_painter))
    this.painter = opt_painter;
  else
    this.painter = new ble.scribble.Painter([]);
};
goog.inherits(ble.Scribble2, ble.scratch.Canvas);

ble.Scribble2.prototype.repaintComplete = function(ctx) {
  ctx.clearRect(0, 0, this.width_px, this.height_px);
  this.painter.drawCompleteTo(ctx);
};

ble.Scribble2.prototype.repaintAt = function(time) {
  return function(ctx) { 
    ctx.clearRect(0, 0, this.width_px, this.height_px);
    this.painter.drawPartialTo(time, ctx);
  };
};

ble.Scribble2.prototype.finishAnimation = function() {
  this.animating = false;
};

ble.Scribble2.prototype.replayAll = function(duration_millis) {
  if(this.animating)
    return;
  this.animating = true;
  var real_duration = this.painter.endTime() - this.painter.startTime();
  if(window.webkitRequestAnimationFrame) {
    this.animateRAF(duration_millis, real_duration);
  } else {
    this.animateInterval(duration_millis, real_duration, 32); 
  }
};

ble.Scribble2.prototype.animateRAF = function(replay_dur, capture_dur) {
  var start = Date.now();
  var redraw = goog.bind(function(now) {
    var delta = now - start;
    if(delta > replay_dur) {
      this.withContext(this.repaintComplete);
      this.finishAnimation();
    } else {
      var effective_time = capture_dur * (delta / replay_dur);
      this.withContext(this.repaintAt(effective_time));
      window.webkitRequestAnimationFrame(redraw);
    }
  }, this);
  redraw(Date.now());
};

ble.Scribble2.prototype.animateInterval = function(replay_dur, capture_dur, interval) {
  var start = Date.now();
  var handle;
  var redraw = goog.bind(function() {
    var now = Date.now();
    var delta = now - start;
    if(delta > replay_dur) {
      this.withContext(this.repaintComplete);
      this.finishAnimation();
      window.clearInterval(handle);
    } else {
      var effective_time = capture_dur * (delta / replay_dur);
      this.withContext(this.repaintAt(effective_time));
    }
  }, this);
  handle = window.setInterval(redraw, interval); 
};

ble.Scribble2.prototype.handleEvent = function(event) {
  goog.base(this, "handleEvent", event);
  if(event.propagationStopped_)
    return;
  var painter = this.painter;
  if(event.type == ble.mocap.EventType.BEGIN) {
    painter.setCurrent(ble.gfx.StrokeReplay.fromMocap(event.capture));
    this.withContext(this.repaintComplete);
  } else if(event.type == ble.mocap.EventType.PROGRESS ||
            event.type == ble.mocap.EventType.CONTROLPOINT) {
    painter.setCurrent(ble.gfx.StrokeReplay.fromMocap(event.capture));
    this.withContext(this.repaintComplete);
  } else if(event.type == ble.mocap.EventType.END) {
    painter.setCurrent(ble.gfx.StrokeReplay.fromMocap(event.capture));
    painter.recordCurrent();
    this.dispatchEvent(event);
  }

};

ble.Scribble2.prototype.enterDocument = function() {
  var motionCapture = new ble.mocap.Stroke();
  goog.events.listen(
      this.getElement(),
      motionCapture.eventTypesOfInterest,
      motionCapture);

  goog.events.listen(
      motionCapture,
      ble.mocap.EventType.ALL,
      this);
  this.mocap_ = motionCapture;
};

ble.Scribble2.prototype.exitDocument = function() {
  if(goog.isDef(this.mocap_)) {
    var motionCapture = this.mocap_;
    goog.events.unlisten(
      this.getElement(),
      motionCapture.eventTypesOfInterest,
      motionCapture);

    goog.events.unlisten(
      motionCapture,
      ble.mocap.EventType.ALL,
      this);
    this.mocap_ = undefined;
  }
};

