goog.require('goog.math.Box');
goog.require('goog.events.EventTarget');

goog.require('ble.scratch.Canvas');
goog.require('ble.scratch.Subcanvas');
goog.require('ble.gfx.StrokeReplay');

goog.provide('ble.scribble.icon');

/**
 * @constructor
 * @extends {goog.events.EventTarget}
 */
ble.scribble.icon.Style = function(color1, color2, strokeWidth) {
  goog.events.EventTarget.call(this);
  this.color1 = color1;
  this.color2 = color2;
  this.strokeWidth = strokeWidth;
};
goog.inherits(ble.scribble.icon.Style, goog.events.EventTarget);

ble.scribble.icon.fixer = function(coordinates) {
  var result = coordinates.slice();
  for(var i = 0; i < result.length; i++) 
    result[i] = Math.round(result[i]);
  return result; 
};

ble.scribble.icon.makeNormalizedStrokeRecorder = function(container, size) {
  var canvas = new ble.scratch.Canvas(size, size);
  canvas.render(container);
  var pxBox = new goog.math.Box(0, size, size, 0);
  var vBox = new goog.math.Box(0, 1, 1, 0);
  var mocap = new ble.mocap.Stroke();
  canvas.getElement().style['border'] = "2px solid red";
  canvas.getElement().style['display'] = "inline-block";
  //Wire mouse events on canvas element to canvas controller...
  goog.events.listen(
      canvas.getElement(),
      mocap.eventTypesOfInterest,
      canvas);
  //Wire canvas events to subcanvas...
  var subcanvas = new ble.scratch.Subcanvas(canvas, pxBox, vBox, true);
  canvas.forwardEvents(subcanvas, mocap.eventTypesOfInterest);
  //Wire subcanvas events to motion capture...
  goog.events.listen(
      subcanvas,
      mocap.eventTypesOfInterest,
      mocap);
  //Wire motion capture events to drawing on the subcanvas... 
  goog.events.listen(
      mocap,
      ble.mocap.EventType.END,
      function(event) {
        
        var stroke = ble.gfx.StrokeReplay.fromMocap(event.capture);
        console.log(stroke);
        subcanvas.withContext(function(context) {
          stroke.drawCompleteTo(context);
        });
      });
}
/*
ble.scribble.icon.Icons = function(style) {
  this.style = style;
  this.subcanvases = [];
};

ble.scribble.icon.Icons.prototype.addIcon = function(container, size, toDraw) {
  var canvas = new ble.scratch.Canvas(size, size);
  canvas.render(container);
  var pxBox = new goog.math.Box(0, size, size, 0);
  var vBox = new goog.math.Box(1.125, 1.125, -0.125, -0.125);
  var subcanvas = new ble.scratch.Subcanvas(canvas, pxBox, vBox);
  subcanvas.withContext(toDraw); 
  this.subcanvases.push([subcanvas, toDraw]);
  return canvas;
};

ble.scribble.icon.Icons.prototype.setIcon = function(index, toDraw) {
  if(index < this.subcanvases.length) {
    this.subcanvases[index][1] = toDraw;
  }
}

ble.scribble.icon.Icons.prototype.redraw = function() {
  for(var i = 0; i < this.subcanvases.length; i++) {
    var subcanvas = this.subcanvases[i][0];
    var toDraw = this.subcanvases[i][1];
    subcanvas.withContext(toDraw);
  }
};

ble.scribble.icon.Icons.prototype.stroke = function(ctx) {
  clearBack(ctx);
  ctx.beginPath();
  ctx.strokeStyle = this.style.color1;
  ctx.lineWidth *= this.style.strokeWidth;
  doStroke(ctx);
};

ble.scribble.icon.Icons.prototype.polyline = function(ctx) {
  clearBack(ctx);
  ctx.beginPath();
  ctx.strokeStyle = this.style.color1;
  ctx.lineWidth *= this.style.strokeWidth;
  doPolyline(ctx, false); 
};

ble.scribble.icon.Icons.prototype.polylineFill = function(ctx) {
  clearBack(ctx);
  ctx.beginPath();
  ctx.strokeStyle = this.style.color1;
  ctx.fillStyle = this.style.color2;
  ctx.lineWidth *= this.style.strokeWidth;
  doPolyline(ctx, true); 
};

ble.scribble.icon.Icons.prototype.erase = function(ctx) {
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

*/
