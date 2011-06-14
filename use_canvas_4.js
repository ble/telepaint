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
container.appendChild(domHelper.createElement("br"));
var link = domHelper.createElement("a");
link.setAttribute("href", "javascript:replayAll()");
link.appendChild(domHelper.createTextNode("replay all"));
container.appendChild(link);

var dehydrate = function() {
  var tmp = scene.complete;
  scene.complete = [];
  canvas.withContext(redrawPolyline);
  return JSON.stringify(tmp);
}

var rehydrate = function(spaceIceCream) {
  var objs = JSON.parse(spaceIceCream);
  scene.complete = [];
  for(var i = 0; i < objs.length; i++) {
    scene.complete.push(ble.mocap.Capture.blessJSONObject(objs[i]));
  }
  canvas.withContext(redrawPolyline);
}


var replayAll = function() {
  var startTime = Date.now();
  var toReplay = [];
  if(scene.beingDrawn != null)
    toReplay.push(beingDrawn);
  while(scene.beingReplayed != null && scene.beingReplayed.length > 0)
    toReplay.push(scene.beingReplayed.pop());
  while(scene.complete.length > 0)
    toReplay.push(scene.complete.pop());
  scene.startTimes = [];
  while(scene.startTimes.length < toReplay.length)
    scene.startTimes.push(startTime);
  scene.beingDrawn = null;
  scene.beingReplayed = toReplay;
  polylineUpdate();
}

=======
>>>>>>> 053f6ddb993d06a9143a42a411f167091c541bf3

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
}
