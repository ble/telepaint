goog.require('goog.math.Size');
goog.require('goog.functions');

goog.require('ble._2d.DrawPart');
goog.require('ble._2d.PipDecorator');
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
    maxLength = Math.max(drawing.length(), maxLength);
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

/**
 * @param {function(ble._2d.DrawPart): ble._2d.DrawPart} f
 * @param {CanvasRenderingContext2D} ctx
 */
ble.scribble.Simultaneous.prototype.prepAndDraw_ = function(f, ctx) {

  var vSize = this.drawingSize;
  var vBox = new goog.math.Box(0, vSize.width, vSize.height, 0);

  var dWidth = this.drawnSize.width;
  var dHeight = this.drawnSize.height;
  for(var i = 0; i < this.drawings.length; i++) {
    var drawing = f(this.drawings[i]);
    var row = Math.floor(i / this.columns);
    var col = i % this.columns;
    var top = row * dHeight;
    var left = col * dWidth;

    var pipBox = new goog.math.Box(top, left+dWidth, top+dHeight, left);
    (new ble._2d.PipDecorator(drawing, vBox, pipBox)).draw(ctx);
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
  if(time >= this.end())
    return this;
  if(time < this.start())
    return ble._2d.nothing;
  var delta = time - this.start();
  var drawable = new ble._2d.Nothing();
  drawable.draw = goog.bind(this.prepAndDraw_, this, function(drawing) {
    return drawing.at(delta);
  });
  return drawable;
}; 


/**
 * @constructor
 * @implements {ble._2d.DrawPart}
 */
ble.scribble.Sequence = function(drawings, drawingSize, traj, moveTime) {
  this.drawings = drawings.slice();
  this.drawingSize = drawingSize;
  this.drawingSpace = new goog.math.Box(0, drawingSize.width, drawingSize.height, 0);
  this.traj = traj;
  this.moveTime = moveTime;
  this.scenes = this.makeScenes();
};

ble.scribble.Sequence.prototype.start = function() {
  return 0;
};

ble.scribble.Sequence.prototype.end = function() {
  return this.scenes[this.scenes.length - 1].time + this.moveTime;
};

ble.scribble.Sequence.prototype.length = function() {
  return this.end();
};

ble.scribble.Sequence.prototype.withStartAt = function(time) {
  throw new Error("unsupported operation");
};

ble.scribble.Sequence.prototype.withLength = function(newLength) {
  var scaleFactor = newLength / this.length();
  return new ble.scribble.Sequence(
      goog.array.map(
        this.drawings,
        function(d) { return d.withLength(d.length() * scaleFactor); }),
      this.drawingSize,
      this.traj,
      this.moveTime * scaleFactor);
};

ble.scribble.Sequence.prototype.makeScenes = function() {
  var lastTime = 0;
  var lastPictures = [];

  var scenes = [];
 
  goog.array.map(
    this.drawings,
    function(drawing) {
      drawing = drawing.withStartAt(lastTime);
      lastPictures.push(drawing);
      while(lastPictures.length > 2)
        lastPictures.shift();

      scenes.push({pictures: lastPictures.slice(), time: lastTime, moving: false}); 
      lastTime += drawing.length();

      scenes.push({pictures: lastPictures.slice(), time: lastTime, moving: true}); 
      lastTime += this.moveTime;
    },
    this);
  return scenes;
};

ble.scribble.Sequence.prototype.draw = function(ctx) {
  var N = this.drawings.length;
  var dN = this.drawings[N-1];
  var dM = this.drawings[N-2];
  var locN = this.traj(1);
  var locM = this.traj(0);
  (new ble._2d.PipDecorator(dN, this.drawingSpace, locN)).draw(ctx);
  (new ble._2d.PipDecorator(dM, this.drawingSpace, locM)).draw(ctx);
};

ble.scribble.Sequence.prototype.drawAt_ = function(time, ctx) {
  time -= this.start();
  var ix = Math.floor(ble.util.rankBinarySearch(
    function(x){ return x.time; },
    this.scenes,
    time));
  if(ix < 0)
    return;
  var scene = this.scenes[ix];
  var pic0 = scene.pictures[0];
  var pic1 = scene.pictures[1];
  var lastPic = goog.isDef(pic1) ? pic1 : pic0;
  var lastLoc;
  if(scene.moving) {
    console.log( (time - scene.time) / this.moveTime);
    lastLoc = this.traj( (time - scene.time) / this.moveTime);
  }
  else
    lastLoc = this.traj(0);

  if(goog.isDef(pic1)) {
    var lastScene = this.scenes[ix-1];
    var prevPip = new ble._2d.MovingPip(
        pic0,
        this.drawingSpace,
        this.traj,
        lastScene.time,
        scene.time);
    prevPip.at(time).draw(ctx);
  }

  var lastPip = new ble._2d.MovingPip(
      lastPic,
      this.drawingSpace,
      this.traj,
      scene.time,
      scene.time + this.moveTime);
  lastPip.at(time).draw(ctx); 
}

ble.scribble.Sequence.prototype.at = function(time) {
  var result = new ble._2d.Nothing();
  result.draw = goog.bind(this.drawAt_, this, time);
  return result;
};
