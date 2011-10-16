


goog.require('ble.scribble.style.caps');

goog.provide('ble.scribble.style.IconPainter');
/**
 * @constructor
 * @param{ble.scribble.style.StylePicker} picker
 */
ble.scribble.style.IconPainter = function(picker) { 
  this.picker = picker;
  this.bigSize = this.picker.height;
  this.smallSize = this.picker.smallSize;
  this.deserialize = goog.bind(ble.scribbleDeserializer.deserialize, ble.scribbleDeserializer);
  this.initIcons();
  
};

/**
 * @param{number} scale
 * @param{Array.<number>} coordinates
 */
ble.scribble.style.IconPainter.prototype.scaleUp = function(scale, coordinates) {
  for(var i = 0; i < coordinates.length; i++) {
    coordinates[i] = Math.round(coordinates[i] * scale);
  }
  return;
};

ble.scribble.style.IconPainter.prototype.flattenTime = function(time, factor) {
  var L = time.length - 1;
  var time0 = time[0];
  var timeF = time[time.length - 1];
  var interval = timeF - time0;

  for(var i = 0; i <= L; i++) {
    time[i] = (time[i] - time0) / interval;
    time[i] = factor * time[i] + (1.0 - factor) * (i / L);
    time[i] = time0 + interval * time[i];
  }
};

ble.scribble.style.IconPainter.prototype.initIcons = function() {
  var makeDefaultStyle = function() { return new ble.gfx.path.PainterVirtual(1, "#000"); };
  var smallStroke = this.deserialize(ble.scribble.style.caps[0]);
  this.scaleUp(this.smallSize, smallStroke.coordinates);
  smallStroke.painter = makeDefaultStyle();
  this.smallStroke = smallStroke;

  var bigStroke = this.deserialize(ble.scribble.style.caps[0]);
  bigStroke = bigStroke.withStartTime(0);
  this.scaleUp(this.bigSize, bigStroke.coordinates);
  this.flattenTime(bigStroke.times, 0.25);
  bigStroke.painter = makeDefaultStyle();
  this.bigStroke = bigStroke;

  var smallPolyline = this.deserialize(ble.scribble.style.caps[1]);
  this.scaleUp(this.smallSize, smallPolyline.coordinates);
  smallPolyline.painter = makeDefaultStyle();
  this.smallPolyline = smallPolyline;

  var bigPolyline = this.deserialize(ble.scribble.style.caps[1]);
  bigPolyline = bigPolyline.withStartTime(0);
  this.scaleUp(this.bigSize, bigPolyline.coordinates);
  bigPolyline.painter = makeDefaultStyle();
  this.bigPolyline = bigPolyline;

  var smallErase = new ble.gfx.EraseReplay(smallStroke.coordinates, smallStroke.times);
  this.smallErase = smallErase;
  var bigErase = new ble.gfx.EraseReplay(bigStroke.coordinates, bigStroke.times, 6);
  this.bigErase = bigErase;
};

ble.scribble.style.IconPainter.prototype.paintStrokeIcon = function(ctx, isBig) {
  this.backdrop(ctx, isBig);
  var stroke = isBig ? this.bigStroke : this.smallStroke;  
  if(isBig)
    stroke.painter = this.picker.getStyle(); 
  stroke.drawCompleteTo(ctx);
};

ble.scribble.style.IconPainter.prototype.paintPolylineIcon = function(ctx, isBig) {
  this.backdrop(ctx, isBig);
  var polyline = isBig ? this.bigPolyline : this.smallPolyline;  
  if(isBig)
    polyline.painter = this.picker.getStyle();
  polyline.drawCompleteTo(ctx);
};

ble.scribble.style.IconPainter.prototype.paintEraseIcon = function(ctx, isBig) {
  ctx.save();
  ctx.fillStyle = "#dd0000";
  var size = isBig? this.bigSize : this.smallSize;
  var delta = 0;
  ctx.fillRect(delta, delta, size-2 * delta, size-2 * delta);
  var erase = isBig? this.bigErase: this.smallErase;
  if(isBig)
    erase.lineWidth = this.picker.getStyle().lineWidth();
  erase.drawCompleteTo(ctx);
  ctx.globalCompositeOperation = "destination-over";
  this.backdrop(ctx, isBig);
  ctx.restore();
};

ble.scribble.style.IconPainter.prototype.paintPolylineFillIcon = function(ctx, isBig) { 
  this.backdrop(ctx, isBig);
  var polyline = isBig ? this.bigPolyline : this.smallPolyline;  
  if(isBig)
    polyline.painter = this.picker.getStyle();
  polyline.painter.fillStyle = this.picker.getFillColor();
  polyline.painter.filled = true;
  polyline.drawCompleteTo(ctx); 
};

ble.scribble.style.IconPainter.prototype.paintIcons = function() {
  var self = this;
  this.picker.smallIcons[0].withContext(function(ctx) {
    self.paintStrokeIcon(ctx, false);
  });
  this.picker.smallIcons[1].withContext(function(ctx) {
    self.paintPolylineIcon(ctx, false);
  });
  this.picker.smallIcons[2].withContext(function(ctx) {
    self.paintEraseIcon(ctx, false);
  });
  this.picker.smallIcons[3].withContext(function(ctx) {
    self.paintPolylineFillIcon(ctx, false);
  });
};

ble.scribble.style.IconPainter.prototype.paintPreview = function() {
  var method = this.picker.getSelectedMethod();
  var mFunc;
  if(method == 0)
    mFunc = this.paintStrokeIcon;
  else if(method == 1)
    mFunc = this.paintPolylineIcon;
  else if(method == 2)
    mFunc = this.paintEraseIcon;
  else if(method == 3)
    mFunc = this.paintPolylineFillIcon;
  else
    throw "bad method";
  var self = this;
  this.picker.bigIcon.withContext(function(ctx) {
    mFunc.call(self, ctx, true);
  });
};

ble.scribble.style.IconPainter.prototype.backdrop = function(ctx, isBig) {
  if(isBig)
    this.backdropOn(ctx, this.bigSize, this.bigSize, Math.floor(this.bigSize / 4)); 
  else
    this.backdropOn(ctx, this.smallSize, this.smallSize, Math.floor(this.smallSize / 4));
};

ble.scribble.style.IconPainter.prototype.backdropOn = function(ctx, width, height, blockSize) {
  ctx.save();
  var wCount = Math.ceil(width / blockSize);
  var hCount = Math.ceil(height / blockSize);
  var color0 = "#ddd";
  var color1 = "#eee";
  for(var i = 0; i < wCount; i++) {
    for(var j = 0; j < hCount; j++) {
      var x = i * blockSize;
      var y = j * blockSize;
      ctx.beginPath();
      ctx.rect(x, y, blockSize, blockSize);
      ctx.fillStyle = ((i + j) % 2 == 0) ? color0 : color1;
      ctx.fill();
    }
  }
  ctx.restore();
};


