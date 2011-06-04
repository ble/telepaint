goog.require('ble.scratch.Canvas');
goog.require('ble.mocap.PixelMocap');
goog.require('goog.events');
goog.require('goog.events.EventType');



var pxWidth = 640;
var pxHeight = 480;
var aspect = pxHeight / pxWidth;

var domHelper = new goog.dom.DomHelper();
var container = domHelper.getElement("outermost");
var canvas = new ble.scratch.Canvas(pxWidth, pxHeight, 1.0);
canvas.render(container);

//var replayCallback = goog.bind(console.log, console);
var replayCallback = function(coords) {
  canvas.withContext(function(context) {
    context.beginPath();
    context.moveTo(coords[0], coords[1]);
    while(coords.length >= 2) {
      context.lineTo(coords.shift(), coords.shift());
    }
    context.stroke();
  });
}

var mocapCallback = function(capture) {
  var replayer = new ble.mocap.Replayer(capture, replayCallback);
  replayer.start();
}
var mocap = new ble.mocap.PixelMocap(mocapCallback);
var relevantTypes = 
  [goog.events.EventType.MOUSEDOWN,
   goog.events.EventType.MOUSEUP,
   goog.events.EventType.MOUSEOUT,
   goog.events.EventType.MOUSEMOVE];

goog.events.listen(canvas.element_, relevantTypes, mocap.handler());
