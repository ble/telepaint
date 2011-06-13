goog.require('ble.scratch.Canvas');
goog.require('ble.mocap.Stroke');
goog.require('goog.events');
goog.require('goog.events.EventType');

var delay = 10;
var pxWidth = 640;
var pxHeight = 480;
var aspect = pxHeight / pxWidth;

var domHelper = new goog.dom.DomHelper();
var container = domHelper.getElement("outermost");
var canvas = new ble.scratch.Canvas(pxWidth, pxHeight);
canvas.render(container);

//Scene graph
var scene = {
  beingDrawn: null,
  beingReplayed: null,
  startTimes: null,
  complete: []
};

//Utility drawing
var strokeCoords = function(ctx, coords) {
  if(coords.length == 0)
    return;
  ctx.beginPath();
  ctx.moveTo(coords[0], coords[1]);
  for(var i = 1; i < coords.length / 2; i++)
    ctx.lineTo(coords[2*i], coords[2*i+1]);
  ctx.stroke();
}

var strokeCoordsWithin = function(ctx, coords, start, last) {
  if(coords.length == 0)
    return;
  ctx.beginPath();
  ctx.moveTo(coords[2*start], coords[2*start+1]);
  for(var i = start+1; i <= last; i++)
    ctx.lineTo(coords[2*i], coords[2*i+1]);
  ctx.stroke();
};

var polylineUntil = function(ctx, coords, control, lastCoord, lastControl) {
  var controlPoints = [];
  for(var i = 0; i <= lastControl; i++) {
    var controlIx = control[i];
    controlPoints.push(coords[controlIx * 2], coords[controlIx * 2 + 1]);
  }
  controlPoints.push(coords[lastCoord * 2], coords[lastCoord * 2 + 1]);
  ctx.beginPath();
  ctx.moveTo(controlPoints[0], controlPoints[1]);
  for(var i = 1; i < controlPoints.length / 2; i++) {
    ctx.lineTo(controlPoints[2 * i], controlPoints[2 * i + 1]);
  }
  ctx.stroke();
};

var polyline = function(ctx, coords, control) {
  if(coords.length < 2 || control.length < 1) return;
  var controlPoints = [];
  for(var i = 0; i < control.length; i++) {
    controlPoints.push(coords[control[i] * 2], coords[control[i] * 2 + 1]);
  }
  controlPoints.push(coords[coords.length - 2], coords[coords.length - 1]);
  strokeCoords(ctx, controlPoints);
};

var redrawPolyline = function(ctx) {
  var now = Date.now();
  ctx.clearRect(0, 0, pxWidth, pxHeight);
  if(!goog.isNull(scene.beingDrawn)) {
    polyline(ctx, scene.beingDrawn.coordinates, scene.beingDrawn.controlTimeIndices);
  }
  for(var i = 0; i < scene.complete.length; i++) {
    polyline(ctx, scene.complete[i].coordinates, scene.complete[i].controlTimeIndices)
  }
  if(!goog.isNull(scene.beingReplayed)) {
    var replayContinue = [];
    var startContinue = [];
    while(scene.beingReplayed.length > 0) {
      var replay = scene.beingReplayed.pop();
      var startTime = scene.startTimes.pop();
      var timeDelta = now - startTime;
      var indices = replay.betweenTimes(0, timeDelta);
      polylineUntil(ctx, replay.coordinates, replay.controlTimeIndices, indices[1], indices[3]);
      console.log(indices);
      var lastTime = replay.times[replay.times.length - 1];
      if(timeDelta <= lastTime) {
        replayContinue.push(replay);
        startContinue.push(startTime);
      } else {
        scene.complete.unshift(replay);
      }
    }
    if(replayContinue.length > 0) {
      scene.beingReplayed = replayContinue;
      scene.startTimes = startContinue;
    } else {
      scene.beingReplayed = null;
      scene.startTimes = null;
    }
  }
};

var polylineUpdate = function() {
  canvas.withContext(redrawPolyline);
  if(!goog.isNull(scene.beingReplayed)) {
    schedulePolylineUpdate();
  }
};

var schedulePolylineUpdate = function() {
  return window.setTimeout(polylineUpdate, delay);
};

var polylineHandler = new goog.events.EventTarget();

polylineHandler.handler0 = function(event) {
  if(event.type == ble.mocap.EventType.BEGIN) {
    scene.beingDrawn = event.capture;
  } else if(event.type == ble.mocap.EventType.PROGRESS ||
            event.type == ble.mocap.EventType.CONTROLPOINT) {
    canvas.withContext(redrawPolyline);
  } else if(event.type = ble.mocap.EventType.END) {
    scene.beingDrawn = null;
    if(goog.isNull(scene.beingReplayed)) {
      scene.beingReplayed = [];
      scene.startTimes = [];
    }
    scene.beingReplayed.push(event.capture);
    scene.startTimes.push(Date.now() - 50);
    polylineUpdate();
  }
};

var polylineCapture = new ble.mocap.Polyline(true);
goog.events.listen(
    canvas.getElement(),
    polylineCapture.eventTypesOfInterest,
    polylineCapture.forwardingListener,
    false,
    polylineCapture);

polylineCapture.addTarget(polylineHandler, ble.mocap.EventType.ALL);
goog.events.listen(
    polylineHandler,
    ble.mocap.EventType.ALL,
    polylineHandler.handler0);
//Redraw strokes at various stages of progress
/*
var redrawStroke = function(ctx) {
  var now = Date.now();
  ctx.clearRect(0, 0, pxWidth, pxHeight);
  if(!goog.isNull(scene.beingDrawn)) {
    strokeCoords(ctx, scene.beingDrawn);
  }
  for(var i = 0; i < scene.complete.length; i++) {
    strokeCoords(ctx, scene.complete[i]);
  }
  if(!goog.isNull(scene.beingReplayed)) {
    var replayRemaining = [];
    var startTimeRemaining = [];
    while(scene.beingReplayed.length > 0) {
      var replay = scene.beingReplayed.pop();
      var startTime = scene.startTimes.pop();
      var timeDelta = now - startTime;
      var indices = replay.betweenTimes(0, timeDelta);
      strokeCoordsWithin(ctx, replay.coordinates, indices[0], indices[1]);
      var lastTime = replay.times[replay.times.length - 1];
      if(timeDelta <= lastTime) {
        replayRemaining.unshift(replay);
        startTimeRemaining.unshift(startTime);
      } else {
        scene.complete.unshift(replay.coordinates);
      }
    }
    if(replayRemaining.length > 0) {
      scene.beingReplayed = replayRemaining;
      scene.startTimes = startTimeRemaining;
    } else {
      scene.beingReplayed = null;
      scene.startTimes = null;
    }
  }
}


var scheduleStrokeUpdate = function() {
  return window.setTimeout(strokeUpdate, delay);
}
var strokeUpdate = function() {
  canvas.withContext(redrawStroke);
  if(!goog.isNull(scene.beingReplayed)) {
    scheduleStrokeUpdate();
  }
}

var strokeHandler = new goog.events.EventTarget();

strokeHandler.handler0 = function(event) {
  if(event.type == ble.mocap.EventType.BEGIN) {
    scene.beingDrawn = event.capture.coordinates;
    canvas.withContext(redrawStroke);
  } else if(event.type == ble.mocap.EventType.PROGRESS ||
            event.type == ble.mocap.EventType.CONTROLPOINT) {
    canvas.withContext(redrawStroke);
  } else if(event.type == ble.mocap.EventType.END) {
    scene.beingDrawn = null;
    if(goog.isNull(scene.beingReplayed)) {
      scene.beingReplayed = [];
      scene.startTimes = [];
    }
    scene.beingReplayed.push(event.capture);
    scene.startTimes.push(Date.now() - 50);
    strokeUpdate();
  }
};

var strokeCapture = new ble.mocap.Stroke();
goog.events.listen(
    canvas.getElement(),
    strokeCapture.eventTypesOfInterest,
    strokeCapture.forwardingListener,
    false,
    strokeCapture);

strokeCapture.addTarget(strokeHandler, ble.mocap.EventType.ALL);
goog.events.listen(
    strokeHandler,
    ble.mocap.EventType.ALL,
    strokeHandler.handler0);
*/
