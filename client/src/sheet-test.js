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

  //Scene graph
  scene = {
    beingDrawn: null,
    beingReplayed: null,
    startTimes: null,
    complete: []
  };

  var redraw = function(ctx) {
    var now = Date.now();
    ctx.clearRect(0, 0, pxWidth, pxHeight);
    if(!goog.isNull(scene.beingDrawn)) {
      ble.gfx.strokeCoords(ctx, scene.beingDrawn.coordinates);
    }
    for(var i = 0; i < scene.complete.length; i++) {
      ble.gfx.strokeCoords(ctx, scene.complete[i].coordinates);
    }
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
    goog.events.listenOnce(client, ble.sheet.EventType.FETCH, function(e) {
      console.log(e);

      var fragments = e.json.fragments;
      for(var i = 0; i < fragments.length; i++) {
        var f = fragments[i];
        if(goog.isDef(f.method) && f.method == "stroke" && goog.isDef(f.data)) {
          scene.complete.push(f.data);
        }
      }
      if(fragments.length > 0) {
        canvas.withContext(redraw);
      }
    });
    client.read().send();
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
      console.log(event.capture);
      var req = client.sheetAppend('stroke', event.capture);

      goog.events.listenOnce(req, goog.net.EventType.COMPLETE,
          function(e){
            _this_ = this;
            if(this.isSuccess()) {
              scene.complete.push(event.capture);
            } else {
              alert('failed to draw stroke');
              canvas.withContext(redraw);
            }
            enabled = true;
            this.dispose();
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
