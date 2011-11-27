
/**
 * @fileoverview Break one canvas element into multiple logical subcanvases
 * which are separately drawable.
 * @author benjaminster@gmail.com (Ben Ellis)
 */

goog.provide('ble.scratch.Canvas');
goog.provide('ble.scratch.Subcanvas');

goog.require('ble.util.hypot');
goog.require('ble._2d.DrawSurface');
goog.require('ble._2d.Drawable');

goog.require('goog.color');
goog.require('goog.math.Box');
goog.require('goog.math.Coordinate');
goog.require('goog.math.Size');
goog.require('goog.graphics.AffineTransform');
goog.require('goog.dom');
goog.require('goog.events');
goog.require('goog.events.EventType');
goog.require('goog.events.EventTarget');
goog.require('goog.events.MouseWheelHandler');
goog.require('goog.style');
goog.require('goog.ui.Component');

var defaultTo = function(default_value, received) {
  return (received !== undefined) ? received : default_value;
};


/**
 *  @type {function(goog.math.Box): goog.math.Size} 
 */
var sizeOfBox = function(box) {
  return new goog.math.Size(box.right - box.left, box.bottom - box.top);
};

/**
 * @interface
 */
ble.scratch.EventRegion = function() {};

/**
 * @param {goog.events.Event} event
 * @return {boolean}
 */
ble.scratch.EventRegion.prototype.inRegion = function(event) {};

/**
 * @return {goog.events.EventTarget}
 */
ble.scratch.EventRegion.prototype.getTarget = function() {};


/**
 * A subsection of a canvas, occupying a certain rectangular region defined
 * in pixel coordinates which may have its own virtual size.
 * @param {ble.scratch.Canvas} parentCanvas
 * @param {goog.math.Box} pixelCoords
 * @param {goog.math.Box=} virtualCoords
 * @param {boolean=} virtualizeOffset
 * @constructor
 * @extends {goog.events.EventTarget}
 * @implements {ble._2d.DrawSurface}
 * @implements {ble.scratch.EventRegion}
 */
ble.scratch.Subcanvas = function(parentCanvas, pixelCoords, virtualCoords, virtualizeOffset) {
  this.parentCanvas_ = parentCanvas;
  this.pixelCoords_ = pixelCoords;

  if(goog.isDef(virtualCoords)) {
    this.virtualCoords_ = virtualCoords;
  } else {
    var pSz = this.pixelSize_();
    this.virtualCoords_ = new goog.math.Box(0, pSz.width, pSz.height, 0);
  }

  this.virtualizeOffset = goog.isDef(virtualizeOffset) ? virtualizeOffset : false;
  goog.events.EventTarget.call(this);
};
goog.inherits(ble.scratch.Subcanvas, goog.events.EventTarget);

ble.scratch.Subcanvas.prototype.pixelSize_ = function() {
  return sizeOfBox(this.pixelCoords_);
};

ble.scratch.Subcanvas.prototype.virtualSize_ = function() {
  return sizeOfBox(this.virtualCoords_);
};

ble.scratch.Subcanvas.prototype.pixelToVirtualRatio_ = function() {
  var pSz = this.pixelSize_();
  var vSz = this.virtualSize_();
  return new goog.math.Size(pSz.width / vSz.width, pSz.height / vSz.height);
};

ble.scratch.Subcanvas.prototype.inRegion = function(event) {
  var coord = new goog.math.Coordinate(event.offsetX, event.offsetY);
  return this.pixelCoords_.contains(coord);
};

ble.scratch.Subcanvas.prototype.getTarget = function() {
  return this;
};

ble.scratch.Subcanvas.prototype.withContext = function(action) {
  var doIt = function(context) {
    //apply clip
    
    context.beginPath();
    context.moveTo(this.pixelCoords_.left, this.pixelCoords_.top);
    context.lineTo(this.pixelCoords_.right, this.pixelCoords_.top);
    context.lineTo(this.pixelCoords_.right, this.pixelCoords_.bottom);
    context.lineTo(this.pixelCoords_.left, this.pixelCoords_.bottom);
    context.closePath();
    context.clip();
    context.translate(this.pixelCoords_.left, this.pixelCoords_.top); 
    var ratio = this.pixelToVirtualRatio_();
    context.scale( ratio.width,
                   ratio.height );
    context.translate(-this.virtualCoords_.left, -this.virtualCoords_.top);
    //just making shit up with this factor here...
    context.lineWidth /= ble.util.hypot( ratio.width / 2,
                                         ratio.height / 2) / Math.sqrt(2);

    action.call(this, context);
  };
  this.parentCanvas_.withContext(goog.bind(doIt, this));
};

ble.scratch.Subcanvas.prototype.affineVirtualToPixel = function() {
  var ratio = this.pixelToVirtualRatio_();
  var transform = new goog.graphics.AffineTransform();
  transform.translate(this.pixelCoords_.left, this.pixelCoords_.top);
  transform.scale(ratio.width, ratio.height);
  transform.translate(-this.virtualCoords_.left, this.virtualCoords_.top);
  return transform;
};


ble.scratch.Subcanvas.prototype.affinePixelToVirtual = function() {
  var ratio = this.pixelToVirtualRatio_();
  var transform = new goog.graphics.AffineTransform();
  transform.translate(this.virtualCoords_.left, this.virtualCoords_.top);
  transform.scale(1.0 / ratio.width, 1.0 / ratio.height);
  transform.translate(-this.pixelCoords_.left, -this.pixelCoords_.top);
  return transform;
};

/**
 * @override
 */
ble.scratch.Subcanvas.prototype.dispatchEvent = function(event) {
  if(goog.isDef(event.offsetX) && goog.isDef(event.offsetY)) {
    this.virtualizeEvent(event);
    if(this.virtualizeOffset) {
      event.offsetX = event.virtualX;
      event.offsetY = event.virtualY;
    } 
  }
  return goog.base(this, "dispatchEvent", event);
};

ble.scratch.Subcanvas.prototype.virtualizeEvent = function(event) {
  var affine = this.affinePixelToVirtual();
  var pixelCoords = [event.offsetX, event.offsetY];
  var virtualCoords = [null, null];
  affine.transform(pixelCoords, 0, virtualCoords, 0, 1);
  event.virtualX = virtualCoords[0];
  event.virtualY = virtualCoords[1];
};

ble.scratch.Subcanvas.prototype.virtualizeListener_replaceOffset = function(listener) {
  var subcanvas = this;
  return function(event) {
    subcanvas.virtualizeEvent(event);
    event.offsetX = event.virtualX;
    event.offsetY = event.virtualY;
    return listener.call(this, event);
  };
};

ble.scratch.Subcanvas.prototype.virtualizeListener = function(listener) {
  var subcanvas = this;
  return function(event) {
    subcanvas.virtualizeEvent(event);
    return listener.call(this, event);
  };
};


/**
 * Simple canvas divisible into subcanvases.
 * @param {number} width_px
 * @param {number} height_px
 * @constructor
 * @extends {goog.ui.Component}
 * @implements {ble._2d.DrawSurface}
 */
ble.scratch.Canvas = function(width_px, height_px) {
  this.width_px = width_px;
  this.height_px = height_px;
  this.element_ = null;

  /**
   * Regions which will receive forwarded events
   * @type {Object.<string,Array.<ble.scratch.EventRegion>>}
   */
  this.regions_ = {};

  var domHelper = new goog.dom.DomHelper();
  goog.ui.Component.call(this, domHelper);
};

goog.inherits(ble.scratch.Canvas, goog.ui.Component);

/**
 * Specify that events of the given type(s) should be passed on to the given
 * event region.
 * @param {ble.scratch.EventRegion} region
 * @param {string|Array.<string>} eventType
 */

ble.scratch.Canvas.prototype.forwardEvents = function(region, eventType) {
  if(goog.isArray(eventType)) {
    for(var i = 0; i < eventType.length; i++) {
      this.forwardEvents(region, eventType[i]);
    }
  } else if(!goog.isNull(eventType)) {
    if(!(eventType in this.regions_))
      this.regions_[eventType] = [];
    var regions = this.regions_[eventType];
    for(var i = 0; i < regions.length; i++) {
      if(regions[i] === region)
        return;
    }
    regions.unshift(region);
  } else {
    throw new TypeError();
  }
};

ble.scratch.Canvas.prototype.handleEvent = function(event) {
  var regions = this.regions_[event.type];
  if(goog.isDef(regions)) {
    for(var i = 0; i < regions.length; i++) {
      var region = regions[i];
      if(region.inRegion(event)) { 
        var result = region.getTarget().dispatchEvent(event);
        if(result === false)
          break; 
      }
    }
  }
};

ble.scratch.Canvas.prototype.withContext = function(action) {
  var context = this.getRawContext();
  context.save();
  context.translate(0.5, 0.5);
  action.call(this, context);
  context.restore();
};

ble.scratch.Canvas.prototype.withClearContext = function(action) {
  var context = this.getRawContext();
  context.clearRect(-1, -1, 2+this.width_px, 2+this.height_px);
  context.save();
  context.translate(0.5, 0.5);
  action.call(this, context);
  context.restore();
};

/**
 * @private
 */
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
};

/**
 * stub
 * @override
 */
ble.scratch.Canvas.prototype.enterDocument = function() {
};

/**
 * Disallow decoration.
 * @override
 */
ble.scratch.Canvas.prototype.canDecorate = function(element) {
  return false;
};

/**
 * override
 */
ble.scratch.Canvas.prototype.decorateInternal = null;






