

var nearestPowerOf10 = function(x) {
  return Math.exp( Math.round( Math.log(x) / Math.LOG10E ) * Math.LOG10E )
}

var getTickSpacing = function(subcanvas, pixelSpacingX, pixelSpacingY) {
  var factors = [1, 2, 5];
  var exactSpacingX = Math.abs(pixelSpacingX / subcanvas.pixelToVirtualRatio.width);
  var exactSpacingY = Math.abs(pixelSpacingY / subcanvas.pixelToVirtualRatio.height);
  var best = [Infinity, Infinity];
  for(var i = 0; i < factors.length; i++) {
    var factor = factors[i];
    var incrementX = factor * nearestPowerOf10(exactSpacingX / factor);
    var incrementY = factor * nearestPowerOf10(exactSpacingY / factor);
    if(Math.abs(incrementX - exactSpacingX) < Math.abs(best[0] - exactSpacingX)) {
      best[0] = incrementX;
    }
    if(Math.abs(incrementY - exactSpacingY) < Math.abs(best[1] - exactSpacingY)) {
      best[1] = incrementY;
    }
  }
  return best;
};

var getTickLocation = function(c0, c1) {
  if(c0 * c1 <= 0)
    return 0;
  else if(Math.abs(c0) < Math.abs(c1))
    return c0;
  else
    return c1;
}

var peek = function(array) {
  return array[array.length - 1];
}

var fillRange = function(start, bound1, bound2, step) {
  var result = [start];
  if(!isFinite(bound1) || !isFinite(bound2) || isNaN(bound1) || isNaN(bound2))
    throw new RangeError();
  var min = Math.min(bound1, bound2);
  var max = Math.max(bound1, bound2);
  while(peek(result) > min) {
    result.push(peek(result) - step);
  }
  result.reverse();
  while(peek(result) < max) {
    result.push(peek(result) + step);
  }
  return result;
}

var axes = function(tickSpacingPixels, lineWidthMutator, strokeStyle, tickLengthPixels) {
  return function(context) {
    var yTickX = getTickLocation(this.virtualCoords_.left, this.virtualCoords_.right);
    var xTickY = getTickLocation(this.virtualCoords_.top, this.virtualCoords_.bottom);
    var spacings = getTickSpacing(this, tickSpacingPixels, tickSpacingPixels);

    var xTickLocs = fillRange(yTickX, this.virtualCoords_.left, this.virtualCoords_.right, spacings[0]);
    var yTickLocs = fillRange(xTickY, this.virtualCoords_.top, this.virtualCoords_.bottom, spacings[1]);
    var xTickHeightVirtual = tickLengthPixels / this.pixelToVirtualRatio.height / 2;
    var yTickWidthVirtual = tickLengthPixels / this.pixelToVirtualRatio.width / 2;
    console.log(context.lineWidth);
    context.save(); {
      console.log(context.lineWidth);
      context.lineWidth = lineWidthMutator(context.lineWidth);
      context.beginPath();
      context.moveTo(this.virtualCoords_.left, 0);
      context.lineTo(this.virtualCoords_.right, 0);
      context.moveTo(0, this.virtualCoords_.top);
      context.lineTo(0, this.virtualCoords_.bottom);

      for(var i = 0; i < xTickLocs.length; i++) {
        context.moveTo(xTickLocs[i], xTickY + xTickHeightVirtual / 2);
        context.lineTo(xTickLocs[i], xTickY - xTickHeightVirtual / 2);
      }

      for(var i = 0; i < yTickLocs.length; i++) {
        context.moveTo(yTickX + yTickWidthVirtual / 2, yTickLocs[i]);
        context.lineTo(yTickX - yTickWidthVirtual / 2, yTickLocs[i]);
      }
      context.strokeStyle = strokeStyle;
      context.stroke();
    } context.restore();

  };
};

var bigAxes = axes(40, function(x) { return x * 0.5; }, "000", 20);

var sinusoid = function(offset, amplitude, frequency) {
  return function(context) {
    var start = this.virtualCoords_.left;
    var end = this.virtualCoords_.right;
    var step = (end - start) / 2000;
    context.beginPath();
    context.moveTo(start, offset + amplitude * sin(frequency * start));
    for(var x = start; x <= end; x += step) {
      context.lineTo(x, offset + amplitude * sin(frequency * x));
    }
  };
};

var domHelper = new goog.dom.DomHelper();
var container = domHelper.getElement("outermost");
var canvas = new ble.scratch.Canvas(640, 480, 1.0);
canvas.render(container);


var fullView = new goog.math.Box(1, 1, -1, -1);
var partial = new goog.math.Box(0.10, 0.15, -0.05, -0.05);

var pipBox = new goog.math.Box(400, 80, 480, 0);
var mainBox = new goog.math.Box(0, 640, 480, 0);

var pip = new ble.scratch.Subcanvas(canvas, pipBox, fullView);
var main = new ble.scratch.Subcanvas(canvas, mainBox, partial);

main.withContext(bigAxes);
