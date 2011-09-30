goog.provide('ble.Scribble');
goog.require('ble.scratch.Canvas');
goog.require('ble.mocap.Stroke');
goog.require('ble.gfx');
goog.require('goog.events');
goog.require('goog.events.EventType');


var pathCoords = ble.gfx.pathPixCoords;
var pathCoordsWithin = ble.gfx.pathPixCoordsWithin;
/**
 * @constructor
 * @extends {ble.scratch.Canvas}
 */
ble.Scribble = function(width, height) {
  ble.scratch.Canvas.call(this, width, height);
  this.scene = {
    beingDrawn: null,
    complete: []
  };
  this.animating = false;
};
goog.inherits(ble.Scribble, ble.scratch.Canvas);

ble.Scribble.prototype.repaintComplete = function(ctx) {
  var scene = this.scene;
  ctx.clearRect(0, 0, this.width_px, this.height_px);
  if(!goog.isNull(scene.beingDrawn)) {
    pathCoords(ctx, scene.beingDrawn.coordinates);
    ctx.stroke();
  }
  for(var i = 0; i < scene.complete.length; i++) {
    pathCoords(ctx, scene.complete[i].coordinates);
    ctx.stroke();
  }
};

ble.Scribble.prototype.repaintAt = function(time) {
  return function(ctx) {
    var scene = this.scene;
    ctx.clearRect(0, 0, this.width_px, this.height_px);
    if(!goog.isNull(scene.beingDrawn)) { 
      pathCoords(ctx, scene.beingDrawn.coordinates);
      ctx.stroke();
    }
    for(var i = 0; i < scene.complete.length; i++) {
      var replay = scene.complete[i];
      if(time > replay.endTime()) {
        pathCoords(ctx, replay.coordinates);
      } else {
        var indices = replay.betweenTimes(-replay.startTime, time-replay.startTime);
        pathCoordsWithin(ctx, replay.coordinates, indices[0], indices[1]); 
      }
        ctx.stroke();
    }
  };
};

ble.Scribble.prototype.finishAnimation = function() {
  this.animating = false;
};

ble.Scribble.prototype.replayAll = function(duration_millis) {
  if(this.animating)
    return;
  this.animating = true;
  var complete = this.scene.complete;
  var real_duration = complete[complete.length - 1].endTime();
  if(window.webkitRequestAnimationFrame) {
    this.animateRAF(duration_millis, real_duration);
  } else {
    this.animateInterval(duration_millis, real_duration, 32); 
  }
};

ble.Scribble.prototype.animateRAF = function(replay_dur, capture_dur) {
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

ble.Scribble.prototype.animateInterval = function(replay_dur, capture_dur, interval) {
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

ble.Scribble.prototype.handleEvent = function(event) {
  goog.base(this, "handleEvent", event);
  if(event.propagationStopped_)
    return;
  var scene = this.scene;
  if(event.type == ble.mocap.EventType.BEGIN) {
    scene.beingDrawn = event.capture;
    this.withContext(this.repaintComplete);
  } else if(event.type == ble.mocap.EventType.PROGRESS ||
            event.type == ble.mocap.EventType.CONTROLPOINT) {
    this.withContext(this.repaintComplete);
  } else if(event.type == ble.mocap.EventType.END) {
    scene.beingDrawn = null; 
    if(scene.complete.length == 0) {
      event.capture.startTime = 0;
    } else {
      event.capture.startTime = scene.complete[scene.complete.length - 1].endTime();
    }
    scene.complete.push(event.capture);
    this.dispatchEvent(event);
  }

};

ble.Scribble.prototype.enterDocument = function() {
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

ble.Scribble.prototype.exitDocument = function() {
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

