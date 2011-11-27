goog.require('goog.math.Size');
goog.require('goog.functions');

goog.require('ble._2d.DrawPart');
goog.require('ble.scribble.Drawing');

goog.provide('ble.scribble.Simultaneous');

/**
 * @constructor
 * @param {number} start
 * @param {Array.<ble.scribble.Drawing>} drawings
 * @param {number} columns
 * @param {goog.math.Size} drawingSize
 * @param {goog.math.Size} drawnSize
 * @implements {ble._2d.DrawPart}
 */
ble.scribble.Simultaneous = function(
    start,
    drawings,
    columns,
    drawingSize,
    drawnSize) {
  this.start_ = start;
  this.drawings = drawings.slice();
  this.columns = columns;
  this.drawingSize = drawingSize;
  this.drawnSize = drawnSize;
};

/**
 * @override
 */
ble.scribble.Simultaneous.prototype.start = function() {
  return this.start_;
};

/**
 * @override
 */
ble.scribble.Simultaneous.prototype.length = function() {
  var maxLength = 0;
  goog.array.forEach(this.drawings, function(drawing) {
    maxLength = Math.max(drawing.length, maxLength);
  });
  return maxLength;
};

/**
 * @override
 */
ble.scribble.Simultaneous.prototype.end = function() {
  return this.start() + this.length();
};

/**
 * @override
 */
ble.scribble.Simultaneous.prototype.withStartAt = function(newStart) {
  return new ble.scribble.Simultaneous(
      newStart,
      this.drawings,
      this.columns,
      this.drawingSize,
      this.drawnSize);
};

ble.scribble.Simultaneous.prototype.withLength = function(newLength) {
  var scale = newLength / this.length();
  var newDrawings = this.drawings.map(function(drawing) {
    return drawing.withLength(drawing.length() * scale);
  });
  return new ble.scribble.Simultaneous(
      this.start(),
      newDrawings,
      this.columns,
      this.drawingSize,
      this.drawnSize); 
};

ble.scribble.Simultaneous.prototype.ratio_ = function() {
  var d0 = this.drawingSize;
  var d1 = this.drawnSize;
  return new goog.math.Size(d1.width / d0.width, d1.height / d0.height); 
};

ble.scribble.Simultaneous.prototype.prepAndDraw_ = function(f, ctx) {
  var ratio = this.ratio_();
  var vSize = this.drawingSize;
  for(var i = 0; i < this.drawings.length; i++) {
    var drawing = f(this.drawings[i]);
    drawing = new ble.scribble.Drawing(0, drawing); 
    var row = Math.floor(i / this.columns);
    var col = i % this.columns;
    var top = row * this.drawnSize.height;
    var left = col * this.drawnSize.width;
    ctx.save();

    ctx.translate(left, top);
    ctx.scale(ratio.width, ratio.height);
    ctx.beginPath();
    ctx.moveTo(0, 0);
    ctx.lineTo(vSize.width, 0);
    ctx.lineTo(vSize.width, vSize.height);
    ctx.lineTo(0, vSize.height);
    ctx.closePath();
    ctx.clip();
    //just making shit up with this factor here...
    ctx.lineWidth /= ble.util.hypot(ratio.width, ratio.height) / Math.sqrt(2);
    drawing.draw(ctx);
    ctx.restore();
  } 
};

/**
 * @override
 */
ble.scribble.Simultaneous.prototype.draw = function(ctx) {
  this.prepAndDraw_(goog.functions.identity, ctx);
};

/**
 * @override
 */
ble.scribble.Simultaneous.prototype.at = function(time) {
  var delta = time - this.start();
  var drawable = new ble._2d.Nothing();
  drawable.draw = goog.bind(this.prepAndDraw_, this, function(drawing) {
    return drawing.at(delta);
  });
  return drawable;
}; 
