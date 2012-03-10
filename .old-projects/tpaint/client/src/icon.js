goog.require('goog.math.Box');
goog.require('goog.events.EventTarget');

goog.require('ble.scratch.Canvas');
goog.require('ble.scratch.Subcanvas');

goog.provide('ble.icon');

/**
 * @constructor
 * @extends {goog.events.EventTarget}
 */
ble.icon.Style = function(color1, color2, strokeWidth) {
  goog.events.EventTarget.call(this);
  this.color1 = color1;
  this.color2 = color2;
  this.strokeWidth = strokeWidth;
};
goog.inherits(ble.icon.Style, goog.events.EventTarget);

ble.icon.Icons = function(style) {
  this.style = style;
  this.subcanvases = [];
};

ble.icon.Icons.prototype.addIcon = function(container, size, toDraw) {
  var canvas = new ble.scratch.Canvas(size, size);
  canvas.render(container);
  var pxBox = new goog.math.Box(0, size, size, 0);
  var vBox = new goog.math.Box(1.125, 1.125, -0.125, -0.125);
  var subcanvas = new ble.scratch.Subcanvas(canvas, pxBox, vBox);
  subcanvas.withContext(toDraw); 
  this.subcanvases.push([subcanvas, toDraw]);
  return canvas;
};

ble.icon.Icons.prototype.setIcon = function(index, toDraw) {
  if(index < this.subcanvases.length) {
    this.subcanvases[index][1] = toDraw;
  }
}

ble.icon.Icons.prototype.redraw = function() {
  for(var i = 0; i < this.subcanvases.length; i++) {
    var subcanvas = this.subcanvases[i][0];
    var toDraw = this.subcanvases[i][1];
    subcanvas.withContext(toDraw);
  }
};

ble.icon.Icons.prototype.stroke = function(ctx) {
  clearBack(ctx);
  ctx.beginPath();
  ctx.strokeStyle = this.style.color1;
  ctx.lineWidth *= this.style.strokeWidth;
  doStroke(ctx);
};

ble.icon.Icons.prototype.polyline = function(ctx) {
  clearBack(ctx);
  ctx.beginPath();
  ctx.strokeStyle = this.style.color1;
  ctx.lineWidth *= this.style.strokeWidth;
  doPolyline(ctx, false); 
};

ble.icon.Icons.prototype.polylineFill = function(ctx) {
  clearBack(ctx);
  ctx.beginPath();
  ctx.strokeStyle = this.style.color1;
  ctx.fillStyle = this.style.color2;
  ctx.lineWidth *= this.style.strokeWidth;
  doPolyline(ctx, true); 
};

ble.icon.Icons.prototype.erase = function(ctx) {
  ctx.beginPath();
  ctx.rect(-0.25, -0.25, 1.5, 15);
  ctx.fillStyle = "48f";
  ctx.fill();

  ctx.lineWidth *= this.style.strokeWidth;
  ctx.globalCompositeOperation = "copy";
  ctx.strokeStyle = "rgba(0, 0, 0, 0.0)"
  ctx.beginPath();
  doStroke(ctx);

  ctx.beginPath();
  ctx.globalCompositeOperation = "destination-over";
  clearBack(ctx); 
};

var wiggle = function(t) { 
    return [t - 0.15 * Math.cos(t * 12),
            t * t - 0.05 * Math.cos(t * 6)];
};

var waggle = function(t) { 
    return [0.5 + 0.5 * Math.exp(-t) * Math.cos(2 * 6.28318 * t),
            t];
};


var clearBack = function(ctx) {
  var N = 4;
  for(var i = -1; i < N+1; i++) {
    for(var j = -1; j < N+1; j++) {
      ctx.beginPath();
      ctx.rect(i/N, j/N, 1/N, 1/N);
      ctx.fillStyle = (i + j) % 2 == 0 ? "#bbb" : "#ddd";
      ctx.fill();
    }
  }
};

var doStroke = function(ctx) {
  ctx.moveTo(0, 0);
  var step = 0.01;
  for(var i = 0; i <= 120; i++) {
    var x = i * step;
    var loc = wiggle(x);
    ctx.lineTo(loc[0], loc[1]);
  }
  ctx.stroke();
};

var doPolyline = function(ctx, fill, color) {
  var loc0 = waggle(0);
  ctx.moveTo(loc0[0], loc0[1]);
  var points = [0, 0.25, 0.4, 0.75, 1.0];
  for(var i = 0; i < points.length; i++) {
    var loc = waggle(points[i]);
    ctx.lineTo(loc[0], loc[1]);
  }
  if(fill) {
    ctx.fill();
  }
  ctx.stroke();
};


