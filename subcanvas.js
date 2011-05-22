
/**
 * @fileoverview Simple framework for breaking one canvas element into
 * multiple, logical subcanvases which are separately drawable.
 * @author benjaminster@gmail.com (Ben Ellis)
 */

goog.provide('ble.scratch.Canvas');
goog.provide('ble.scratch.Subcanvas');

goog.require('goog.dom');
goog.require('goog.events.Event');
goog.require('goog.style');
goog.require('goog.ui.Component');

var defaultTo = function(default_value, received) {
  return (received !== undefined) ? received : default_value;
};

var hypot = function(x, y) {
  return Math.sqrt(x * x + y * y);
}


/**
 * A subsection of a canvas, occupying a certain rectangular region defined
 * in pixel coordinates which may have its own virtual size.
 * @param {ble.scratch.Canvas} parentCanvas
 * @param {Array.<number>} pixelCoords
 * @param {Array.<number>=} virtualCoords
 * @constructor
 */
ble.scratch.Subcanvas = function(parentCanvas, pixelCoords, virtualCoords) {
  this.parentCanvas_ = parentCanvas;
  this.pixelCoords_ = pixelCoords;
  this.pixelWidth_ = pixelCoords[2] - pixelCoords[0];
  this.pixelHeight_ = pixelCoords[3] - pixelCoords[1];
  if(virtualCoords !== undefined) {
    var virtualCoords_ = virtualCoords.slice();
    if(virtualCoords_.length === 2) {
      virtualCoords_.unshift(0, 0);
    }
    this.virtualCoords_ = virtualCoords_;
  } else {
    this.virtualCoords_ = [0, 0, this.pixelWidth_, this.pixelHeight_];
  }
  this.virtualWidth_ = this.virtualCoords_[2] - this.virtualCoords_[0];
  this.virtualHeight_ = this.virtualCoords_[3] - this.virtualCoords_[1];
};

ble.scratch.Subcanvas.prototype.withContext = function(action) {
  var context = this.parentCanvas_.getRawContext();
  context.save();

  //apply clip
  
  context.beginPath();
  context.moveTo(this.pixelCoords_[0], this.pixelCoords_[1]);
  context.lineTo(this.pixelCoords_[2], this.pixelCoords_[1]);
  context.lineTo(this.pixelCoords_[2], this.pixelCoords_[3]);
  context.lineTo(this.pixelCoords_[0], this.pixelCoords_[3]);
  context.closePath();
  context.clip();
  context.translate(this.pixelCoords_[0], this.pixelCoords_[1]); 
  context.scale(
      this.pixelWidth_ / this.virtualWidth_,
      this.pixelHeight_ / this.virtualHeight_);
  context.translate(-this.virtualCoords_[0], -this.virtualCoords_[1]);
  context.lineWidth *= hypot(this.virtualWidth_ / this.pixelWidth_,
                             this.virtualHeight_ / this.pixelHeight_);


  action.call(this, context);

  context.restore();
}

/**
 * Simple canvas divisible into subcanvases.
 * @param {number} width_px
 * @param {number} height_px
 * @constructor
 * @extends {goog.ui.Component}
 */
ble.scratch.Canvas = function(width_px, height_px, width_logical, aspect_logical) {
  this.width_px = width_px;
  this.height_px = height_px;
  this.element_ = null;

  var domHelper = new goog.dom.DomHelper();
  goog.ui.Component.call(this, domHelper);
};

goog.inherits(ble.scratch.Canvas, goog.ui.Component);

ble.scratch.Canvas.prototype.withContext = function(action) {
  var context = this.getRawContext();
  context.save();
  action.call(this, context);
  context.restore();
};


ble.scratch.Canvas.prototype.getRawContext = function() {
  return this.element_.getContext("2d");
};


/**
 * Create the DOM element corresponding to this Canvas
 * @override
 */
ble.scratch.Canvas.prototype.createDom = function() {
  if(this.element_ === null) {
    var domHelper = this.getDomHelper();
    var attrib = {"width": this.width_px, "height": this.height_px};
    this.element_ = domHelper.createDom("canvas", attrib);
  }
}

/**
 * stub
 * @override
 */
ble.scratch.Canvas.prototype.enterDocument = function() {
}

/**
 * Disallow decoration.
 * @override
 */
ble.scratch.Canvas.prototype.canDecorate = function(element) {
  return false;
}

/**
 * override
 */
ble.scratch.Canvas.prototype.decorateInternal = null;






