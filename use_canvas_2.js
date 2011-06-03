goog.require('ble.scratch.Canvas');
goog.require('ble.scratch.Subcanvas');
goog.require('ble.curves.Renderer');
goog.require('ble.curves.Angle');
goog.require('ble.curves.CurvedPart');

var domHelper = new goog.dom.DomHelper();
var container = domHelper.getElement("outermost");
var canvas = new ble.scratch.Canvas(640, 480, 1.0);
canvas.render(container);

var arc = function(angle, radius) {
  return new ble.curves.CurvedPart(Math.abs(angle * radius), 1.0 / radius);
}
/*
var radii = [];
for(var i = 0; i < 100; i++) {
  var sign = (i - 1 + 4) % 4 > 2 ? -1 : 1;
  radii.push(0.01 * i * sign);
}
var parts = radii.map(function(radius) { return arc((radius < 0) ? -Math.PI : Math.PI, radius); });
*/
var parts = [];
var signs = [];
for(var i = 0; i < 1000; i++) { 
  var sign = i % 3 == 0 ? 1 : -1;
  var radius = 0.001 * i;
  var angle = Math.PI * sign / 2.5;
  var length = Math.abs(angle) * radius;
  parts.push(new ble.curves.CurvedPart(length, sign / radius));
  signs.push(sign);
}


var renderer = new ble.curves.Renderer(parts, [0, 0], 0);
var coords = renderer.renderCurve();

var fullCanvas = new goog.math.Box(0, 640, 480, 0);
var view = new goog.math.Box(0.75, 1.0, -0.75, -1.0);
var subcanvas = new ble.scratch.Subcanvas(canvas, fullCanvas, view);
subcanvas.withContext( function(context) {
  context.beginPath();
  context.moveTo(view.left, 0);
  context.lineTo(view.right, 0);
  context.moveTo(0, view.top);
  context.lineTo(0, view.bottom);
  context.stroke();

  context.beginPath();
  context.moveTo(coords[0], coords[1]);
  for(var i = 1; i < coords.length / 2; i++) {
    context.lineTo(coords[2*i], coords[2*i + 1]);
  }
  context.strokeStyle = "f00";
  context.stroke();
});
