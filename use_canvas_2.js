goog.require('ble.scratch.Canvas');
goog.require('ble.scratch.Subcanvas');
goog.require('ble.curves.Renderer');
goog.require('ble.curves.Angle');
goog.require('ble.curves.CurvedPart');


var arc = function(angle, radius) {
  return new ble.curves.CurvedPart(Math.abs(angle * radius), 1.0 / radius);
}

var parts = [];
var signs = [];
for(var i = 0; i < 2000; i++) { 
  var sign = i % 3 == 0 ? 1 : -1;
  var radius = 0.0004 * i;
  var angle = (Math.PI * sign / 3.35);
  var length = Math.abs(angle) * radius;
  parts.push(new ble.curves.CurvedPart(length, sign / radius));
  signs.push(sign);
}


var renderer = new ble.curves.Renderer(parts, [0, 0], 0);
var coords = renderer.renderCurve();


var pxWidth = 1920;
var pxHeight = 1200;
var aspect = pxHeight / pxWidth;

var domHelper = new goog.dom.DomHelper();
var container = domHelper.getElement("outermost");
var canvas = new ble.scratch.Canvas(pxWidth, pxHeight, 1.0);
canvas.render(container);
canvas.withContext( function(context) {
  context.fillStyle = "000";
  context.fillRect(0, 0, pxWidth, pxHeight);
});

var fullCanvas = new goog.math.Box(0, pxWidth, pxHeight, 0);
var view = new goog.math.Box(aspect, 1.0, -aspect, -1.0);
var subcanvas = new ble.scratch.Subcanvas(canvas, fullCanvas, view);

var renderCoords = function(lineWidthMutator, strokeStyle, parts, origin, angle, coordsPerTime, completer) {
  var allCoords = (new ble.curves.Renderer(parts, origin, angle)).renderCurve();
  var coordIndex = 0;
  return function(context) {
    var coordsThisTime = Math.min(Math.max(0, allCoords.length - coordIndex), coordsPerTime) / 2;
    var startCoord = Math.max(0, coordIndex - 2);
    var coords = allCoords.slice(startCoord, coordIndex + 2 * coordsPerTime);
    coordIndex += 2 * coordsThisTime;
    context.beginPath();
    context.moveTo(coords[0], coords[1]);
    for(var i = 1; i < coords.length / 2; i++) {
      context.lineTo(coords[2*i], coords[2*i + 1]);
    }
    context.lineWidth = lineWidthMutator(context.lineWidth);
    context.strokeStyle = strokeStyle;
    context.stroke();
    if(coordIndex == allCoords.length)
      completer.done();
    console.log(allCoords.length - coordIndex);
  }
};

var id = function(x) { return x; };
var theColor = "cdc";


var completer = function() {
  return {
    "done": function() {
      clearInterval(this.id); 
    },
    "id": null
  };
}



var completer0 = completer();
var render0 = renderCoords(id, theColor, parts, [-0.25, 0.25], 0, 10, completer0);
var id = setInterval(function() { subcanvas.withContext(render0); }, 15);
completer0.id = id;
/*

var render1 = renderCoords(id, theColor, parts, [0.25,-0.25], Math.PI / 6, 1000, null);
var render2 = renderCoords(id, theColor, parts, [-0.25,-0.25], -Math.PI / 6, 1000, null);
var render3 = renderCoords(id, theColor, parts, [0.25,0.25], Math.PI / 3, 1000, null);
subcanvas.withContext( render3 );
subcanvas.withContext( render3 );
subcanvas.withContext( render3 );
subcanvas.withContext( render3 );
subcanvas.withContext( render2 );
subcanvas.withContext( render2 );
subcanvas.withContext( render2 );
subcanvas.withContext( render1 );
subcanvas.withContext( render1 );
subcanvas.withContext( render0 );
*/
