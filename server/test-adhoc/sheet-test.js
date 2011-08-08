goog.require('ble.scratch.Canvas');
goog.require('ble.mocap.Stroke');
goog.require('goog.events');
goog.require('goog.events.EventType');

goog.provide('ble.sheet.run_test');

var pxWidth = 640;
var pxHeight = 480;
var aspect = pxHeight / pxWidth;
var JSON;

var domHelper;
var container;
var canvas;
ble.sheet.run_test = function() {
  domHelper = new goog.dom.DomHelper();
  container = domHelper.getElement("outermost");
  canvas = new ble.scratch.Canvas(pxWidth, pxHeight);
  canvas.render(container);
  container.appendChild(domHelper.createElement("br"));
  var link = domHelper.createElement("a");
  link.setAttribute("href", "javascript:replayAll()");
  link.appendChild(domHelper.createTextNode("replay all"));
  container.appendChild(link);


var replayAll = function() {
  var startTime = Date.now();
  var toReplay = [];
  if(scene.beingDrawn != null)
    toReplay.push(scene.beingDrawn);
  while(scene.beingReplayed != null && scene.beingReplayed.length > 0)
    toReplay.push(scene.beingReplayed.pop());
  while(scene.complete.length > 0)
    toReplay.push(scene.complete.pop());
  scene.startTimes = [];
  while(scene.startTimes.length < toReplay.length)
    scene.startTimes.push(startTime);
  scene.beingDrawn = null;
  scene.beingReplayed = toReplay;
  update();
}

var dehydrate = function() {
  var tmp = scene.complete;
  scene.complete = [];
  canvas.withContext(redraw);
  return JSON.stringify(tmp);
}

var rehydrate = function(spaceIceCream) {
  var objs = JSON.parse(spaceIceCream);
  scene.complete = [];
  for(var i = 0; i < objs.length; i++) {
    scene.complete.push(ble.mocap.Capture.blessJSONObject(objs[i]));
  }
  canvas.withContext(redraw);
}


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
}


//Redraw strokes at various stages of progress
var redraw = function(ctx) {
  var now = Date.now();
  ctx.clearRect(0, 0, pxWidth, pxHeight);
  if(!goog.isNull(scene.beingDrawn)) {
    strokeCoords(ctx, scene.beingDrawn.coordinates);
  }
  for(var i = 0; i < scene.complete.length; i++) {
    strokeCoords(ctx, scene.complete[i].coordinates);
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
        scene.complete.unshift(replay);
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

var delay = 10;

var scheduleUpdate = function() {
  return window.setTimeout(update, delay);
}
var update = function() {
  canvas.withContext(redraw);
  if(!goog.isNull(scene.beingReplayed)) {
    scheduleUpdate();
  }
}


var mocapHandler = new goog.events.EventTarget();

mocapHandler.handler0 = function(event) {
  if(event.type == ble.mocap.EventType.BEGIN) {
    scene.beingDrawn = event.capture;
    canvas.withContext(redraw);
  } else if(event.type == ble.mocap.EventType.PROGRESS ||
            event.type == ble.mocap.EventType.CONTROLPOINT) {
    canvas.withContext(redraw);
  } else if(event.type == ble.mocap.EventType.END) {
    scene.beingDrawn = null;
    if(goog.isNull(scene.beingReplayed)) {
      scene.beingReplayed = [];
      scene.startTimes = [];
    }
    scene.beingReplayed.push(event.capture);
    scene.startTimes.push(Date.now() - 50);
    update();
  }
};

var motionCapture = new ble.mocap.Stroke();
goog.events.listen(
    canvas.getElement(),
    motionCapture.eventTypesOfInterest,
    motionCapture.forwardingListener,
    false,
    motionCapture);

motionCapture.addTarget(mocapHandler, ble.mocap.EventType.ALL);
goog.events.listen(
    mocapHandler,
    ble.mocap.EventType.ALL,
    mocapHandler.handler0);
};
