/**
 * @fileoverview Snowflake-drawing app! Yay!
 * @author benjaminster@gmail.com (Ben Ellis)
 */
goog.require('ble.scratch.Canvas');
goog.require('ble.scratch.Subcanvas');
goog.require('ble.mocap.PixelMocap');
goog.require('goog.math.Box');
goog.require('goog.events');
goog.require('goog.events.EventType');

goog.provide('ble.SnowflakeApp');

/**
 * App 'scene graph'
 * @param {ble.SnowflakeApp} app
 * @constructor
 */
ble.SnowflakeScene = function(app) {
  this.app = app;
  this.mocaps = [];
  this.beingDrawn = [];
  this.showTriangle = true;
  this.showSymmetries = true;
  this.drawAll = true;
  this.drawUntilTime = false;
};

ble.SnowflakeScene.prototype.draw = function(context) {
  if(this.showSymmetries) {
    var symmetry = this.app.symmetry;
    var angle = 2 * Math.PI / symmetry;
    for(var i = 0; i < this.app.symmetry; i++) {
      context.save();
      context.rotate(angle * i);
      this.draw0(context);
      context.scale(-1.0, 1.0);
      this.draw0(context);
      context.restore();
    }
  } else {
      this.draw0(context);

  }
}

ble.SnowflakeScene.prototype.draw0 = function(context) {
  if(this.showTriangle) {
    this.app.wedgePath(context)
    context.stroke();
  }

  if(this.drawAll) {
    for(var i = 0; i < this.beingDrawn.length; i++) {
      var coords = this.beingDrawn[i].slice();
      context.beginPath();
      if(coords.length >= 2)
        context.moveTo(coords.shift(), coords.shift());
      while(coords.length >= 2) {
        context.lineTo(coords.shift(), coords.shift());
      }
      context.stroke();
    }
  }
};

ble.SnowflakeScene.prototype.addStroke = function(event) {
  if(event.type == goog.events.EventType.MOUSEDOWN) 
    this.beingDrawn.unshift([]);
  this.beingDrawn[0].push(event.virtualX, event.virtualY);
  this.app.redraw();
};

ble.SnowflakeScene.prototype.addMocap = function(event) {
  this.showTriangle = false;
  this.showSymmetries = true;
  this.drawAll = true;
  this.app.inputMode = false;
  this.app.redraw();
};

/**
 * App entry point.
 * @param {Element} container element within which to render the app
 * @constructor
 */
ble.SnowflakeApp = function(container) {
  this.container = container;
  this.hasBeenRun = false;
  this.canvas = null;
};

ble.SnowflakeApp.prototype.run = function() {
  if(this.hasBeenRun)
    throw Error('already running');
  this.hasBeenRun = true;
  this.canvas = new ble.scratch.Canvas(this.pxWidth, this.pxHeight);
  this.canvas.render(this.container);
  var pBox = new goog.math.Box(0, this.pxWidth, this.pxHeight, 0);
  var vBox = new goog.math.Box(this.aspect(), 1, -this.aspect(), -1);
  this.vCanvas = new ble.scratch.Subcanvas(this.canvas, pBox, vBox);
  this.vCanvas.withContext(goog.bind(this.drawWedge, this));
  this.canvas.forwardEvents([
      goog.events.EventType.MOUSEDOWN,
      goog.events.EventType.MOUSEMOVE,
      goog.events.EventType.MOUSEUP]);
  this.canvas.addSubcanvas(this.vCanvas);
  this.scene = new ble.SnowflakeScene(this);
  this.mocap = new ble.mocap.PixelMocap(
      goog.bind(this.scene.addStroke, this.scene),
      goog.bind(this.scene.addMocap, this.scene));
  this.inputMode = true;
  goog.events.listen(
    this.vCanvas,
    [goog.events.EventType.MOUSEDOWN,
     goog.events.EventType.MOUSEMOVE,
     goog.events.EventType.MOUSEUP], goog.bind(this.handler, this));
};

ble.SnowflakeApp.prototype.redraw = function() {
  this.canvas.withContext(bindFlip(this, function(context) {
    context.clearRect(0, 0, this.pxWidth, this.pxHeight);
  }));
  this.vCanvas.withContext(goog.bind(this.scene.draw, this.scene));
}

var bindFlip = function(boundThis, fn) {
  return goog.bind(fn, boundThis);
};

/**
 * Event-handling state for a SnowflakeApp
 * @param {ble.SnowflakeApp} app the app which uses this handler
 * @constructor
 */
ble.SnowflakeHandler = function(app) {
  this.app = app;
}

ble.SnowflakeApp.prototype.handler = function(event) {
  if(this.inputMode) {
    var isInWedge;
    this.vCanvas.withContext(bindFlip(this, function(context) {
      this.wedgePath(context);
      isInWedge = context.isPointInPath(event.offsetX, event.offsetY);
    }));
    if(isInWedge)
      this.mocap.handler()(event);
  }
}

ble.SnowflakeApp.prototype.pxWidth = 640;
ble.SnowflakeApp.prototype.pxHeight = 480;
ble.SnowflakeApp.prototype.aspect = function() {
  return this.pxHeight / this.pxWidth;
};

ble.SnowflakeApp.prototype.symmetry = 6;

ble.SnowflakeApp.prototype.wedgePath = function(context) {
  var angle = Math.PI / this.symmetry;
  var radius = Math.min(1.0, this.aspect());
  var loc0 = [0, radius];
  var loc1 = [Math.sin(angle) * radius, Math.cos(angle) * radius];
  var loc2 = [0, 0];
  context.beginPath();
  context.moveTo(loc0[0], loc0[1]);
  context.lineTo(loc1[0], loc1[1]);
  context.lineTo(loc2[0], loc2[1]);
  context.closePath();
}

ble.SnowflakeApp.prototype.drawWedge = function(context) {
  this.wedgePath(context);
  context.stroke();
};


