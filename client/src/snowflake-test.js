goog.require('ble.scratch.Canvas');
goog.require('ble.scratch.Subcanvas');

goog.require('ble.snowflake.Client');
goog.require('ble.snowflake.Painter');
goog.require('ble.snowflake.EventType');

goog.require('goog.dom.DomHelper');
goog.require('goog.math.Box');

ble.snowflake.run_test = function() { 
  var domHelper = new goog.dom.DomHelper();
  var container = domHelper.getElement("outermost");
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
  canvas.render(container);
  subcanvas = new ble.scratch.Subcanvas(canvas, subRectPixels, subRectVirtual, true);

  })();

  var motionCapture = new ble.mocap.Polyline(true);
  var mouseTypes = motionCapture.eventTypesOfInterest;
  canvas.forwardEvents(subcanvas, mouseTypes);
  goog.events.listen(canvas.getElement(), mouseTypes, canvas);


  //Create the undo link
  var linkUndo = domHelper.createDom('a', null, 'undo');
  container.appendChild(domHelper.createElement("br"));
  container.appendChild(linkUndo);



  //Figure out the URL to interact with
  var flakeUrl;
  (function() {
    var parts = window.location.toString().split("/");
    var flakeName = parts[parts.length - 1];
    flakeUrl = "/snowflake/" + flakeName;
  })();

  //Create the state and the client objects
  var state = new ble.snowflake.State(subcanvas);
  var client = new ble.snowflake.Client(flakeUrl);

  //Wire the client to the state
  goog.events.listen(
    client,
    [ble.snowflake.EventType.INIT, ble.snowflake.EventType.UPDATE],
    state);

  //wire the motion capture object to receive virtual coordinates events from
  //the subcanvas.
  goog.events.listen(
      subcanvas,
      motionCapture.eventTypesOfInterest,
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
      var method = getCurrentMethod();
      event.capture.method = method;
      state.setCurrentInteraction({'method': method, 'data': event.capture});
      state.repaint();
    }

    else if(event.type == ble.mocap.EventType.PROGRESS ||
            event.type == ble.mocap.EventType.CONTROLPOINT) {
      state.repaint();
    }

    else if(event.type == ble.mocap.EventType.END) {
      state.setCurrentInteraction(null);
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
    drawEnabled = false;
    var req = client.rpcUndo();
    goog.events.listenOnce(
      req,
      goog.net.EventType.COMPLETE,
      function() { drawEnabled = true; });
    req.send();
  });
  client.readState().send();
};
