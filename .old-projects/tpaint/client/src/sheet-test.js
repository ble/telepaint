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
var toggleMethod;
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
  container.appendChild(domHelper.createElement("br"));

  var methodIndex = 0;
  var methods = ["stroke", "erase"];
  var currentMethod = methods[methodIndex];
  var getCurrentMethod = function() { return methods[methodIndex]; };
  var getMethodLabel = function() { return "currently: " + getCurrentMethod(); };
  var toggler = domHelper.createDom('a', {'href': 'javascript:toggleMethod()'}, getMethodLabel());
  container.appendChild(toggler);
  toggleMethod = function() {
    methodIndex = (methodIndex + 1) % methods.length;
    console.log([methodIndex, getMethodLabel()]);
    goog.dom.setTextContent(toggler, getMethodLabel());
  }

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

  var drawOne = function(ctx, item) {
    var coords;
    if(goog.isDef(item.data))
      coords = item.data.coordinates;
    if(!goog.isDef(coords))
      coords = item.coordinates;

    if(item.method == "stroke") { 
      ble.gfx.strokeCoords(ctx, coords);
    } else if(item.method == "erase") {
      ctx.save();
      ctx.globalCompositeOperation = "copy";
      ctx.strokeStyle = "rgba(0,0,0,0.0)";
      ble.gfx.strokeCoords(ctx, coords);
      ctx.restore(); 
    }
  }
  redraw = function(ctx) {
    var now = Date.now();
    ctx.clearRect(0, 0, pxWidth, pxHeight);
    
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
        goog.bind(drawOne, this, ctx));

    if(!goog.isNull(scene.beingDrawn)) {
      drawOne(ctx, scene.beingDrawn);
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

  //create motion capture and wire it to the canvas
  var motionCapture = new ble.mocap.Stroke();
  goog.events.listen(
      canvas.getElement(),
      motionCapture.eventTypesOfInterest,
      motionCapture);

  //create the handler for motion capture events and
  //wire it to the motion capture

  var mocapHandler = (function() {
    var enabled = true;
    return function(event) {
      if(!enabled)
        return;

      if(event.type == ble.mocap.EventType.BEGIN) {
        event.capture.method = getCurrentMethod();
        scene.beingDrawn = event.capture;
        canvas.withContext(redraw);
      } else if(event.type == ble.mocap.EventType.PROGRESS ||
                event.type == ble.mocap.EventType.CONTROLPOINT) {
        canvas.withContext(redraw);
      } else if(event.type == ble.mocap.EventType.END) {
        scene.beingDrawn = null; 
        enabled = false;
        var req = client.append(event.capture.method, event.capture);
        goog.events.listenOnce(req, goog.net.EventType.COMPLETE,
            function(e){
              enabled = true;
            });
        req.send();
      }
    };
  }());

  goog.events.listen(
      motionCapture,
      ble.mocap.EventType.ALL,
      mocapHandler);
};
