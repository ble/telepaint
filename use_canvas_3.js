goog.provide('ble.use_canvas_3');

goog.require('ble.Scribble');
goog.require('ble.mocap.EventType');
goog.require('ble.mocap.Capture');
goog.require('goog.events');

goog.require('goog.storage.mechanism.HTML5LocalStorage');
goog.require('goog.iter');


ble.use_canvas_3 = function() {
  var pxWidth = 640;
  var pxHeight = 480;

  var dom = new goog.dom.DomHelper();
  var container = dom.getElement("outermost");
  var canvas = new ble.Scribble(pxWidth, pxHeight);
  canvas.render(container);

  var console = window.console;
  var JSON = window.JSON;


  var storage = new goog.storage.mechanism.HTML5LocalStorage();
  var key = "scribble";
  var saveState = function() { 
    storage.set(key, JSON.stringify(canvas.scene.complete));
  };

  var restoreState = function() {
    try {
      var complete = JSON.parse(storage.get(key));
    } catch(error) {
      console.log("Unparseable JSON found in local storage; clearing.");
      storage.remove(key);
      complete = null;
    }

    if(goog.isNull(complete))
      canvas.scene.complete = [];

    var result = [];
    for(var i = 0; i < complete.length; i++) {
      try {
        var toAdd = ble.mocap.Capture.blessJSONObject(complete[i]);
        result.push(toAdd);
      } catch(error) {
        console.log("Failed to convert item to motion capture, ignoring.");
      }
    };
    canvas.scene.complete = result;
  };


  restoreState();
  canvas.withContext(canvas.repaintComplete);

  var debug = true;
  if(debug) {
    window.canvas = canvas;
    window.saveState = saveState;
    window.restoreState = restoreState;
    window.storage = storage;
  }
  goog.events.listen(canvas, ble.mocap.EventType.END, function(e) {
    saveState();
  });

  var replay = dom.createDom("a", {'href': 'javascript:void(null)'}, ["replay"]);
  var clear = dom.createDom("a", {'href': 'javascript:void(null)'}, ["clear"]);
  var controls = dom.createDom("div", null, [replay, " , ", clear]);
  container.appendChild(controls);

  goog.events.listen(
    replay,
    goog.events.EventType.CLICK,
    function() {
      var replayLength = canvas.scene.complete.length * 500;
      canvas.replayAll(replayLength);
    });
  goog.events.listen(
    clear,
    goog.events.EventType.CLICK,
    function() {
      canvas.scene.complete = [];
      saveState();
      canvas.withContext(canvas.repaintComplete);
    });

};

goog.exportSymbol('ble.use_canvas_3', ble.use_canvas_3);
