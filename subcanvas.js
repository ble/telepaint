
/**
 * @fileoverview Simple framework for breaking one canvas element into
 * multiple, logical subcanvases which are separately drawable.
 * @author benjaminster@gmail.com (Ben Ellis)
 */

goog.provide('ble.scratch.Canvas');
goog.provide('ble.scratch.Subcanvas');

goog.require('goog.math.Box');
goog.require('goog.math.Coordinate');
goog.require('goog.math.Size');
goog.require('goog.dom');
goog.require('goog.events');
goog.require('goog.events.EventType');
goog.require('goog.style');
goog.require('goog.ui.Component');

var defaultTo = function(default_value, received) {
  return (received !== undefined) ? received : default_value;
};

var hypot = function(x, y) {
  return Math.sqrt(x * x + y * y);
}

/**
 *  @type {function(goog.math.Box): goog.math.Size} 
 */
var sizeOfBox = function(box) {
  return new goog.math.Size(box.right - box.left, box.bottom - box.top);
};

/**
 * A subsection of a canvas, occupying a certain rectangular region defined
 * in pixel coordinates which may have its own virtual size.
 * @param {ble.scratch.Canvas} parentCanvas
 * @param {goog.math.Box} pixelCoords
 * @param {goog.math.Box=} virtualCoords
 * @constructor
 * @extends {goog.events.EventTarget}
 */
ble.scratch.Subcanvas = function(parentCanvas, pixelCoords, virtualCoords) {
  this.parentCanvas_ = parentCanvas;
  this.pixelCoords_ = pixelCoords;
  this.pixelSize_ = sizeOfBox(this.pixelCoords_);

  if(virtualCoords !== undefined) {
    this.virtualCoords_ = virtualCoords;
    this.virtualSize_ = sizeOfBox(this.virtualCoords_);
  } else {
    this.virtualSize_ = this.pixelSize_;
    this.virtualCoords_ = new goog.math.Box(0, this.virtualSize_.width, this.virtualSize_.height, 0);
  }
  this.pixelToVirtualRatio = new goog.math.Size(
    this.pixelSize_.width / this.virtualSize_.width,
    this.pixelSize_.height / this.virtualSize_.height);
  goog.events.EventTarget.call(this);
};
goog.inherits(ble.scratch.Subcanvas, goog.events.EventTarget);

ble.scratch.Subcanvas.prototype.withContext = function(action) {
  var context = this.parentCanvas_.getRawContext();
  context.save();

  //apply clip
  
  context.beginPath();
  context.moveTo(this.pixelCoords_.left, this.pixelCoords_.top);
  context.lineTo(this.pixelCoords_.right, this.pixelCoords_.top);
  context.lineTo(this.pixelCoords_.right, this.pixelCoords_.bottom);
  context.lineTo(this.pixelCoords_.left, this.pixelCoords_.bottom);
  context.closePath();
  context.clip();
  context.translate(this.pixelCoords_.left, this.pixelCoords_.top); 
  context.scale( this.pixelToVirtualRatio.width,
                 this.pixelToVirtualRatio.height );
  context.translate(-this.virtualCoords_.left, -this.virtualCoords_.top);
  context.lineWidth *= hypot( this.pixelToVirtualRatio.width,
                              this.pixelToVirtualRatio.height);

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

  /**
   * Subcanvases registered to this canvas.
   * @type {Array.<ble.scratch.Subcanvas>}
   */
  this.subcanvases_ = [];

  /**
   * Listener object which forwards events to subcanvases
   */
  this.forwardingListener_ = null;

  var domHelper = new goog.dom.DomHelper();
  goog.ui.Component.call(this, domHelper);
};

goog.inherits(ble.scratch.Canvas, goog.ui.Component);

/**
 * Enable event forwarding to subcanvases for a particular event type.
 */
ble.scratch.Canvas.prototype.forwardEvents = function(type) {
  if(this.forwardingListener_ === null) {
    this.forwardingListener_ = function(event) {
      var coord = new goog.math.Coordinate(event.offsetX, event.offsetY);
      for(var i = 0; i < this.subcanvases_.length; i++) {
        var subcanvas = this.subcanvases_[i];
        if(subcanvas.pixelCoords_.contains(coord)) {
          var result = subcanvas.dispatchEvent(event);
          if(result === false) {
            return false;
          }
        }
      }
    };
  }
  goog.events.listen(this.element_, type, this.forwardingListener_, false, this);
};

/**
 * Register a subcanvas
 * @param {ble.scratch.Subcanvas|Array.<ble.scratch.Subcanvas>} subcanvas
 */
ble.scratch.Canvas.prototype.addSubcanvas = function(subcanvas) {
  if(goog.isArray(subcanvas)) {
    for(var i = 0; i < subcanvas.length; i++) {
      this.addSubcanvas(subcanvas[i]);
    }
  } else {
    this.subcanvases_.unshift(subcanvas)
  }
}



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






