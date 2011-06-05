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
 * App entry point.
 * @param {Element} container element within which to render the app
 * @constructor
 */
ble.SnowflakeApp = function(container) {
  this.container = container;
  this.hasBeenRun = false;
  this.canvas = null;
}

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
};

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
  var angle = Math.PI / 6;
  var radius = Math.min(1.0, this.aspect());
  var loc0 = [0, radius];
  var loc1 = [Math.sin(angle) * radius, Math.cos(angle) * radius];
  var loc2 = [0, 0];
  context.beginPath();
  context.moveTo(loc0[0], loc0[1]);
  context.lineTo(loc1[0], loc1[1]);
  context.lineTo(loc2[0], loc2[1]);
  context.closePath();
  context.stroke();
};
