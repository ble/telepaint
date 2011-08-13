goog.require('ble.scratch.Canvas');
goog.require('ble.mocap.Stroke');
goog.require('ble.sheet.Client');
goog.require('ble.sheet.EventType');
goog.require('goog.events');
goog.require('goog.events.EventType');
goog.require('goog.net.EventType');

goog.provide('ble.sheet.run_test');

var scene;
var client;
var canvas;
var redraw;
var undo;

var _this_;

ble.sheet.run_test = function() {
  var pxWidth = 640;
  var pxHeight = 480;
  var aspect = pxHeight / pxWidth;

  var domHelper = new goog.dom.DomHelper();
  var container = domHelper.getElement("outermost");
  canvas = new ble.scratch.Canvas(pxWidth, pxHeight);
  canvas.render(container);
  canvas.element_.style['background-color'] = "#FFD";
  container.appendChild(domHelper.createElement("br"));
  var linkOut = domHelper.createDom('a', {'href': 'javascript:undo()'}, 'undo');
  container.appendChild(linkOut);

  undo = function() {
    client.undo().send();
  };

  //Scene graph
  scene = {
    beingDrawn: null,
    beingReplayed: null,
    startTimes: null,
    complete: []
  };

  redraw = function(ctx) {
    var now = Date.now();
    ctx.clearRect(0, 0, pxWidth, pxHeight);
    if(!goog.isNull(scene.beingDrawn)) {
      ble.gfx.strokeCoords(ctx, scene.beingDrawn.coordinates);
    }
    
    var complete = client.fragments.getValues();
    goog.array.forEach(
      complete,
      function(item) {
        if(item.method == "undo") {
          client.fragments.remove(item['data']['clientTimeToUndo']);
          client.fragments.remove(item['clientTime']);
        }
      });
    complete = client.fragments.getValues();
    goog.array.forEach(
        complete,
        function(item) {
          if(item.method = "stroke") {
            ble.gfx.strokeCoords(ctx, item.data.coordinates);
          }
        });

    if(!goog.isNull(scene.beingReplayed)) {
      var replayRemaining = [];
      var startTimeRemaining = [];
      while(scene.beingReplayed.length > 0) {
        var replay = scene.beingReplayed.pop();
        var startTime = scene.startTimes.pop();
        var timeDelta = now - startTime;
        var indices = replay.betweenTimes(0, timeDelta);
        ble.gfx.strokeCoordsWithin(ctx, replay.coordinates, indices[0], indices[1]);
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

  {
    var sheetUrl = (function() {
      var parts = window.location.toString().split("/");
      var sheetName = parts[parts.length - 1];
      return "/sheet/" + sheetName;
    })(); 
    client = new ble.sheet.Client(sheetUrl);
    goog.events.listenOnce(
      client,
      ble.sheet.EventType.FETCH,
      goog.bind(canvas.withContext, canvas, redraw));
    client.read().send();

    goog.events.listen(
      client,
      ble.sheet.EventType.UPDATE,
      goog.bind(canvas.withContext, canvas, redraw));
  }

  var mocapHandler = new goog.events.EventTarget();
  var enabled = true;
  mocapHandler.handler0 = function(event) {
    if(!enabled)
      return;

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
      enabled = false;
      var req = client.append('stroke', event.capture);

      

      goog.events.listenOnce(req, goog.net.EventType.COMPLETE,
          function(e){
            enabled = true;
          });
      req.send();
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
