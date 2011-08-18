goog.require('goog.math.Box');
goog.require('ble.scratch.Canvas');
goog.require('ble.scratch.Subcanvas');

goog.provide('ble.icon');

ble.icon.render = function(container, icon) {
  var bSize = 40;
  var canvas = new ble.scratch.Canvas(bSize, bSize);
  canvas.render(container);
  var pxBox = new goog.math.Box(0, bSize, bSize, 0);
  var vBox = new goog.math.Box(1.125, 1.125, -0.125, -0.125);
  var subcanvas = new ble.scratch.Subcanvas(canvas, pxBox, vBox);
  subcanvas.withContext(icon); 
  return canvas;
};

var wiggle = function(t) { 
    return [t - 0.15 * Math.cos(t * 12),
            t * t - 0.05 * Math.cos(t * 6)];
};

var waggle = function(t) { 
    return [0.5 + 0.5 * Math.exp(-t) * Math.cos(2 * 6.28318 * t),
            t];
};


var clearBackStyle = function(ctx) {
  var N = 4;
  for(var i = -1; i < N+1; i++) {
    for(var j = -1; j < N+1; j++) {
      ctx.beginPath();
      ctx.rect(i/N, j/N, 1/N, 1/N);
      ctx.fillStyle = (i + j) % 2 == 0 ? "#bbb" : "#ddd";
      ctx.fill();
    }
  }
  ctx.beginPath();
  ctx.strokeStyle = "#000";
  ctx.lineWidth = 0.075; 
}

var doStroke = function(ctx) {
  ctx.moveTo(0, 0);
  var step = 0.01;
  for(var i = 0; i <= 120; i++) {
    var x = i * step;
    var loc = wiggle(x);
    ctx.lineTo(loc[0], loc[1]);
  }
  ctx.stroke();
}

var doPolyline = function(ctx, fill, color) {
  var loc0 = waggle(0);
  ctx.moveTo(loc0[0], loc0[1]);
  var points = [0, 0.25, 0.4, 0.75, 1.0];
  for(var i = 0; i < points.length; i++) {
    var loc = waggle(points[i]);
    ctx.lineTo(loc[0], loc[1]);
  }
  if(fill) {
    ctx.fillStyle = color;
    ctx.fill();
  }
  ctx.stroke();

}

ble.icon.stroke = function(ctx) {
  clearBackStyle(ctx);
  doStroke(ctx);
};

ble.icon.polyline = function(ctx) {
  clearBackStyle(ctx);
  doPolyline(ctx, false);
}

ble.icon.polylineFill = function(ctx) {
  clearBackStyle(ctx);
  doPolyline(ctx, true, "#48f");
};

ble.icon.erase = function(ctx) {
  ctx.beginPath();
  ctx.rect(-0.25, -0.25, 1.5, 15);
  ctx.fillStyle = "48f";
  ctx.fill();

  ctx.lineWidth = 0.3;
  ctx.globalCompositeOperation = "copy";
  ctx.strokeStyle = "rgba(0, 0, 0, 0.0)"
  ctx.beginPath();
  doStroke(ctx);

  ctx.beginPath();
  ctx.globalCompositeOperation = "destination-over";
  clearBackStyle(ctx);
};



