


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
  var makeDefaultStyle = function() { return new ble._2d.path.PainterVirtual(1, "#000"); };
  var smallStroke = this.deserialize(ble.scribble.style.caps[0]);
  this.scaleUp(this.smallSize, smallStroke.coordinates);
  smallStroke.painter = makeDefaultStyle();
  this.smallStroke = smallStroke;

  var bigStroke = this.deserialize(ble.scribble.style.caps[0]);
  bigStroke = bigStroke.withStartAt(0);
  this.scaleUp(this.bigSize, bigStroke.coordinates);
  this.flattenTime(bigStroke.times, 0.25);
  bigStroke.painter = makeDefaultStyle();
  this.bigStroke = bigStroke;

  var smallPolyline = this.deserialize(ble.scribble.style.caps[1]);
  this.scaleUp(this.smallSize, smallPolyline.coordinates);
  smallPolyline.painter = makeDefaultStyle();
  this.smallPolyline = smallPolyline;

  var smallPolylineFill = this.deserialize(ble.scribble.style.caps[1]);
  this.scaleUp(this.smallSize, smallPolylineFill.coordinates);
  smallPolylineFill.painter = makeDefaultStyle();
  this.smallPolylineFill = smallPolylineFill;

  var bigPolyline = this.deserialize(ble.scribble.style.caps[1]);
  bigPolyline = bigPolyline.withStartAt(0);
  this.scaleUp(this.bigSize, bigPolyline.coordinates);
  bigPolyline.painter = makeDefaultStyle();
  this.bigPolyline = bigPolyline;

  var smallErase = new ble._2d.EraseReplay(
    smallStroke.coordinates,
    smallStroke.startTime,
    smallStroke.times);
  this.smallErase = smallErase;
  var bigErase = new ble._2d.EraseReplay(
      bigStroke.coordinates,
      smallStroke.startTime,
      bigStroke.times);
  this.bigErase = bigErase;
};

ble.scribble.style.IconPainter.prototype.paintStrokeIcon = function(ctx, isBig) {
  this.clear(ctx, isBig);
  this.backdrop(ctx, isBig);
  var stroke = isBig ? this.bigStroke : this.smallStroke;  
  if(isBig)
    stroke.painter = this.picker.getStyle(); 
  stroke.draw(ctx);
};

ble.scribble.style.IconPainter.prototype.paintPolylineIcon = function(ctx, isBig) {
  this.clear(ctx, isBig);
  this.backdrop(ctx, isBig);
  var polyline = isBig ? this.bigPolyline : this.smallPolyline;  
  if(isBig)
    polyline.painter = this.picker.getStyle();
  polyline.draw(ctx);
};

ble.scribble.style.IconPainter.prototype.paintEraseIcon = function(ctx, isBig) {
  this.clear(ctx, isBig);
  ctx.save();
  ctx.fillStyle = "#dd0000";
  var size = isBig? this.bigSize : this.smallSize;
  var delta = 0;
  ctx.fillRect(delta, delta, size-2 * delta, size-2 * delta);
  var erase = isBig? this.bigErase: this.smallErase;
  if(isBig)
    erase.lineWidth = this.picker.getStyle().lineWidth;
  erase.draw(ctx);
  ctx.globalCompositeOperation = "destination-over"
  this.backdrop(ctx, isBig);
  ctx.restore();
};

ble.scribble.style.IconPainter.prototype.paintPolylineFillIcon = function(ctx, isBig) { 
  this.clear(ctx, isBig);
  this.backdrop(ctx, isBig);
  var polyline = isBig ? this.bigPolyline : this.smallPolylineFill;  
  if(isBig)
    polyline.painter = this.picker.getStyle();
  polyline.painter.fillStyle = this.picker.getFillColor();
  polyline.painter.filled = true;
  polyline.draw(ctx); 
};

ble.scribble.style.IconPainter.prototype.paintIcons = function() {
  var self = this;
  this.picker.smallIcons[0].withClearContext(function(ctx) {
    self.paintStrokeIcon(ctx, false);
  });
  this.picker.smallIcons[1].withClearContext(function(ctx) {
    self.paintPolylineIcon(ctx, false);
  });
  this.picker.smallIcons[2].withClearContext(function(ctx) {
    self.paintEraseIcon(ctx, false);
  });
  this.picker.smallIcons[3].withClearContext(function(ctx) {
    self.paintPolylineFillIcon(ctx, false);
  });
  var selection = this.picker.getSelectedMethod();
  if(selection >= 0 && selection < 4) {
    this.picker.smallIcons[selection].withContext(function(ctx) {
      ctx.save();
      ctx.beginPath();
      ctx.rect(0,0,self.smallSize,self.smallSize);
      ctx.lineWidth=10;
      ctx.strokeStyle="#48f";
      ctx.stroke(); 
      ctx.restore();
    });
  }
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
  this.picker.bigIcon.withClearContext(function(ctx) {
    mFunc.call(self, ctx, true);
  });
};

ble.scribble.style.IconPainter.prototype.clear = function(ctx, isBig) {
  if(isBig)
    ctx.clearRect(ctx, -4, -4, this.bigSize+2, this.bigSize+2);
  else
    ctx.clearRect(ctx, -4, -4, this.smallSize+2, this.smallSize+2);

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


