goog.require('ble.scratch.Canvas');
goog.require('ble.scratch.Subcanvas');

goog.require('ble.snowflake.Client');
goog.require('ble.snowflake.Painter');
goog.require('ble.snowflake.EventType');

goog.require('goog.dom.DomHelper');
goog.require('goog.math.Box');

ble.snowflake.run_test = function() {
  var canvas, subcanvas;
  //Create the canvas and subcanvas and wire them together so that the
  //subcanvas will emit events when its.
  
  //Determine size of canvas and size of subcanvas
  (function() {
  var aspect = 0.75;
  var width = 800;
  var height = aspect * width;
  var scaleUp = 1.1;

  var subRectPixels = new goog.math.Box(0, width, height, 0);
  var subRectVirtual = new goog.math.Box(scaleUp, scaleUp/aspect, -scaleUp, -scaleUp/aspect);
  canvas = new ble.scratch.Canvas(width, height);
  subcanvas = new ble.scratch.Subcanvas(canvas, subRectPixels, subRectVirtual);

  })();

  var motionCapture = new ble.mocap.Polyline(true);
  var mouseTypes = motionCapture.eventTypesOfInterest;
  canvas.forwardEvents(subcanvas, mouseTypes);
  goog.events.listen(canvas, mouseTypes, canvas.forwardingListener);

  //Create the undo link
  var domHelper = new goog.dom.DomHelper();
  var linkUndo = domHelper.createDom('a', null, 'undo');

  //Figure out the URL to interact with
  var flakeUrl;
  (function() {
    var parts = window.location.toString().split("/");
    var flakeName = parts[parts.length - 1];
    flakeUrl = "/snowflake/" + flakeName;
  })();

  //Create the painter and the client objects
  var painter = new ble.snowflake.Painter(subcanvas);
  var client = new ble.snowflake.Client(flakeUrl);

  //Wire the client to the painter
  goog.events.listen(
    client,
    ble.snowflake.EventType.INIT,
    goog.bind(painter.initClient, painter, client));

  goog.events.listen(
    client,
    ble.snowflake.EventType.UPDATE,
    goog.bind(painter.updateClient, painter, client));

  //wire the motion capture object to receive virtual coordinates events from
  //the subcanvas.
  goog.events.listen(
      subcanvas,
      motionCapture.eventTypesOfInterest,
      subcanvas.virtualizeListener_replaceOffset(motionCapture.forwardingListener),
      false,
      motionCapture);

  var getCurrentMethod = function() {
    return "erase-polyline";
  };

  //create the draw handler and wire it to receive events from the motion
  //capture object
  var drawEnabled = true;
  var drawHandler = function(event) {

    if(!drawEnabled)
      return;

    if(event.type == ble.mocap.EventType.BEGIN) {
      event.capture.method = getCurrentMethod();
      painter.setCurrentInteraction(event.capture);
      painter.repaint();
    }

    else if(event.type == ble.mocap.EventType.PROGRESS ||
            event.type == ble.mocap.EventType.CONTROLPOINT) {
      painter.repaint();
    }

    else if(event.type == ble.mocap.EventType.END) {
      painter.setCurrentInteraction(null);
      drawEnabled = false;
      var req = client.rpcAppend(event.capture.method, event.capture);
      goog.events.listenOnce(
        req,
        goog.net.EventType.COMPLETE,
        function() { drawEnabled = true; });
      req.send(); 
    }

  };
  goog.events.listen(
      motionCapture,
      ble.mocap.EventType.ALL,
      drawHandler);

  //attach undo event handler
  goog.events.listen(linkUndo, goog.events.EventType.CLICK, function(e) {
    if(!drawEnabled)
      return;
    var req = client.rpcUndo();
    goog.events.listenOnce(
      req,
      goog.net.EventType.COMPLETE,
      function() { drawEnabled = true; });
    req.send();
  });
  //attach canvas and undo link to the document
  var container = domHelper.getElement("outermost");
  canvas.render(container);
  container.appendChild(domHelper.createElement("br"));
  container.appendChild(linkUndo);
};
