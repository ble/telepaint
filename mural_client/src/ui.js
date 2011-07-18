
goog.require('goog.dom.DomHelper');
goog.require('goog.ui.Component');
goog.require('goog.color.alpha');
goog.require('goog.structs.Map');
goog.require('goog.events.Event');
goog.require('goog.string.format');

goog.provide('ble.mural.SplitImage');
goog.provide('ble.mural.SplitEvent');
goog.provide('ble.mural.EventTypes');
goog.provide('ble.mural.ClippedImage');

/**
 * @constructor
 * @extends {goog.ui.Component}
 */

ble.mural.SplitImage = function(img_url, n_rows, n_cols) {
  this.img_url = img_url;
  this.rows = n_rows;
  this.cols = n_cols;
  this.element_ = null;
  this.rectContainer_ = null;
  this.img_ = null;
  this.map_ = null;
  this.imgSrcSize_ = null;
  goog.ui.Component.call(this, new goog.dom.DomHelper());
};
goog.inherits(ble.mural.SplitImage, goog.ui.Component);

ble.mural.SplitImage.prototype.colors = [[255, 255, 255, 0.5], [192, 192, 192, 0.5]];
ble.mural.SplitImage.prototype.borderWidth = 5;
ble.mural.SplitImage.prototype.borderColor = "rgba(0, 0, 0, 0.5)";
ble.mural.SplitImage.prototype.maxDims = [640, 480];

ble.mural.SplitImage.prototype.getWidth = function() {
  return this.img_.width;
};

ble.mural.SplitImage.prototype.getHeight = function() {
  return this.img_.height;
};

ble.mural.SplitImage.prototype.getBoxWidth = function() {
  return this.img_.width / this.cols;
};

ble.mural.SplitImage.prototype.getBoxHeight = function() {
  return this.img_.height / this.rows;
};

ble.mural.SplitImage.prototype.getSourceBoxSize = function() {
  return {
    width: this.imgSrcSize_.width / this.cols,
    height: this.imgSrcSize_.height / this.rows
  };
    
};

ble.mural.SplitImage.prototype.rectStyle_ = function(row, col) {
  var colorIx = (row + col) % this.colors.length;
  var color = goog.color.alpha.rgbaStyle_(this.colors[colorIx]);
  var result = "";
  var tmp;
  result = 
    ('position:absolute;' +
    'background-color:' + color + ';' +
    'border-style:solid;' +
    'border-width:' + this.borderWidth + ';' +
    'border-color:' + this.borderColor + ';' +
    'width:' + (this.getBoxWidth() - 2 * this.borderWidth) + ';' +
    'height:' + (this.getBoxHeight() - 2 * this.borderWidth) + ';');
  return result;
};

ble.mural.SplitImage.prototype.constrainSize = function(width, height) {
  var constraintAspect = this.maxDims[0] / this.maxDims[1];
  var aspect = width / height;
  if(aspect >= 1) {
    return [this.maxDims[0], this.maxDims[0] / aspect];
  } else if(aspect <= constraintAspect) {
    return [aspect * this.maxDims[1], this.maxDims[1]];
  } else {
    throw "NaN in size constraint.";
  }
};

ble.mural.SplitImage.prototype.createDom = function() {
  if(this.element_ === null) {
    var img = this.dom_.createDom("img", {src: this.img_url}); 
    this.imgSrcSize_ = {
      width: img.width,
      height: img.height
    };
    var size = this.constrainSize(img.width, img.height);
    img.width = size[0];
    img.height = size[1];
    this.img_ = img;


    if(this.rectContainer_ !== null)
      this.dom_.removeNode(this.rectContainer_);    
    this.rectContainer_ = this.makeRects_();
    this.element_ = this.dom_.createDom("div", null, this.img_, this.rectContainer_); 
    
  }
};

ble.mural.SplitImage.prototype.enterDocument = function() {
  goog.ui.Component.prototype.enterDocument.call(this);
  goog.events.listen(
      this.rectContainer_,
      goog.events.EventType.MOUSEOVER,
      goog.bind(
        function(e) {
          var elt = e.getBrowserEvent().srcElement;
          var rowCol = this.map_.get(goog.getUid(elt));
          if(rowCol) {
            var event = new ble.mural.SplitEvent(rowCol[0], rowCol[1], this);
            this.dispatchEvent(event);
          }
        },
        this));
};

ble.mural.SplitImage.prototype.makeRects_ = function() {

  var boxHeight = this.getBoxHeight();
  var boxWidth = this.getBoxWidth();
  var boxMap = new goog.structs.Map();
  var style = "width:" + this.getWidth() + ';' +
              "height:" + this.getHeight() + ';' +
              "top:0px;left:0px;position:absolute;";
  var rectContainer = this.dom_.createDom("div", {'style': style});
  for(var row = 0; row < this.rows; row++) {
    for(var col = 0; col < this.cols; col++) {
      var top = boxHeight * row;
      var left = boxWidth * col;
      var style = 'top:' + top + ';' +
                  'left:' + left + ';' + 
                  this.rectStyle_(row, col);
      var rect = this.dom_.createDom("div", {'style': style, 'row': row, 'col': col});
      this.dom_.append(rectContainer, rect);
      boxMap.set(goog.getUid(rect), [row, col]);
    }
  }
  this.map_ = boxMap;
  return rectContainer;
};

/**
 * @constructor
 * @extends {goog.events.Event}
 */
ble.mural.SplitEvent = function(row, col, split) {
  goog.events.Event.call(this, ble.mural.EventTypes.RECTANGLEHOVER);
  this.row = row;
  this.col = col;
  this.split = split;
};
goog.inherits(ble.mural.SplitEvent, goog.events.Event);

ble.mural.SplitEvent.prototype.getSourceRect = function() {
  var src = this.split.getSourceBoxSize();
  return {
    left: src.width * this.col,
    top: src.height * this.row,
    right: src.width * (this.col + 1),
    bottom: src.height * (this.row + 1),
    width: src.width,
    height: src.height
  };
};
        


ble.mural.EventTypes = {
  RECTANGLEHOVER: 'rectanglehover'
};


/**
 * @constructor
 * @extends {goog.ui.Component}
 */

ble.mural.ClippedImage = function(img_src, box) {
  this.img_src = img_src;
  this.box = box;
  goog.ui.Component.call(this, new goog.dom.DomHelper());
};

goog.inherits(ble.mural.ClippedImage, goog.ui.Component);

ble.mural.ClippedImage.prototype.createDom = function() {
  if(this.element_ == null) {
    this.img_ = this.dom_.createDom("img", {'src': this.img_src});
    this.element_ = this.dom_.createDom("div", null, this.img_);
    this.updateBox_();
  }
};

ble.mural.ClippedImage.prototype.setBox = function(box) {
  this.box = box;
  this.updateBox_();
}

ble.mural.ClippedImage.prototype.updateBox_ = function() {
  var cStyle = this.element_.style;
  var box = this.box;
  cStyle.width = box.right - box.left;
  cStyle.height = box.bottom - box.top;
  cStyle.position = "relative";
  var iStyle = this.img_.style;
  iStyle.clip = goog.string.format(
    "rect(%dpx,%dpx,%dpx,%dpx)",
    box.top,
    box.right,
    box.bottom,
    box.left);
  iStyle.left = -box.left;
  iStyle.top = -box.top;
  iStyle.position = "absolute";
}
