goog.require('goog.dom.DomHelper');
goog.require('goog.math.Box');
goog.require('goog.events');
goog.require('goog.events.EventTarget');
goog.require('goog.events.EventType');
goog.require('goog.ui.HsvaPalette');
goog.require('goog.ui.Slider');
goog.require('goog.color.alpha');

goog.require('ble.scratch.Canvas');
goog.require('ble.scratch.Subcanvas');
goog.require('ble.gfx.StrokeReplay');
goog.require('ble.json.PrettyPrinter');
goog.require('ble.scribble.style.caps');

goog.provide('ble.scribble.backdropOn');

goog.provide('ble.scribble.icon.makeNormalizedStrokeRecorder');
goog.provide('ble.scribble.icon.makeNormalizedPolylineRecorder');
goog.provide('ble.scribble.style.StylePicker');

ble.scribble.backdropOn = function(ctx, width, height, blockSize) {
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

/**
 * @constructor
 * @extends {goog.ui.Component}
 */
ble.scribble.style.StylePicker = function() {
  goog.ui.Component.call(this);
  this.smallSize = Math.floor(this.height / 2);
  this.initIconReplays();
};
goog.inherits(ble.scribble.style.StylePicker, goog.ui.Component);


ble.scribble.style.StylePicker.prototype.height = 187;

ble.scribble.style.StylePicker.prototype.initIconReplays = function() {
  this.smallCaps = [];
  this.bigCaps = [];

  var smallStroke = ble.scribbleDeserializer.deserialize(ble.scribble.style.caps[0]);
  smallStroke.coordinates = ble.scribble.icon.scaleUp(this.smallSize, smallStroke.coordinates);
  var bigStroke = ble.scribbleDeserializer.deserialize(ble.scribble.style.caps[0]);
  bigStroke = bigStroke.withStartTime(0);
  bigStroke.coordinates = ble.scribble.icon.scaleUp(this.height, bigStroke.coordinates);
  bigStroke.times = ble.scribble.icon.flattenTime(bigStroke.times, 0.1);

  var smallPolyline = ble.scribbleDeserializer.deserialize(ble.scribble.style.caps[1]);
  smallPolyline.coordinates = ble.scribble.icon.scaleUp(this.smallSize, smallPolyline.coordinates);
  var bigPolyline = ble.scribbleDeserializer.deserialize(ble.scribble.style.caps[1]);
  bigPolyline = bigPolyline.withStartTime(0);
  bigPolyline.coordinates = ble.scribble.icon.scaleUp(this.height, bigPolyline.coordinates);
  bigPolyline.times = ble.scribble.icon.flattenTime(bigPolyline.times, 0.1);

  this.smallCaps.push(smallStroke);
  this.smallCaps.push(smallPolyline);


  this.bigCaps.push(bigStroke);
  this.bigCaps.push(bigPolyline);
};

ble.scribble.style.StylePicker.prototype.repaintAll = function() {
  if(!this.isInDocument())
    return;
  var self = this;
  for(var i = 0; i < this.smallIcons.length; i++) {
    this.smallIcons[i].withContext(function(ctx) {
      ctx.clearRect(0, 0, self.smallSize, self.smallSize);
      ble.scribble.backdropOn(ctx, self.smallSize, self.smallSize, Math.round(self.smallSize / 4));
      if(self.smallCaps.length > i) {
        self.smallCaps[i].drawCompleteTo(ctx);
      }
    });
  }
  this.bigIcon.withContext(function(ctx) {
      ctx.clearRect(0, 0, self.height, self.height);
      ble.scribble.backdropOn(ctx, self.height, self.height, Math.round(self.height / 5));
      var i = self.getSelectedMethod();
      if(self.bigCaps.length > i) {
        self.bigCaps[i].painter = self.getStyle();
        self.bigCaps[i].drawCompleteTo(ctx);
      }

  });
};

ble.scribble.style.StylePicker.prototype.animateBig = function() {
  if(this.animationHandle_ != null)
    this.cancelAnimation();
  this.animationStart = Date.now();
  this.animationDuration = 2500;
  this.animationFn = goog.bind(function() {
    var now = Date.now();  
    var delta = (now - this.animationStart) / this.animationDuration;
    if(delta >= 1) {
      this.repaintAll();
      this.cancelAnimation();
    } else {
      var i = this.getSelectedMethod();
      if(this.bigCaps.length > 1) {
        var bigCap = this.bigCaps[i];
        bigCap.painter = this.getStyle();
        var capTime = bigCap.startTime() + (bigCap.endTime() - bigCap.startTime()) * delta;
        this.bigIcon.withContext(goog.bind(function(ctx) {
          ctx.clearRect(0, 0, this.height, this.height);
          ble.scribble.backdropOn(ctx, this.height, this.height, Math.round(this.height / 5));
          bigCap.drawPartialTo(capTime, ctx); 
        }, this));
      }
    }
  }, this);
  this.animationHandle_ = window.setInterval(this.animationFn, 16);
}

ble.scribble.style.StylePicker.prototype.cancelAnimation = function() {
  if(this.animationHandle_ == null)
    return;
  window.clearInterval(this.animationHandle_);
  this.animationHandle_ = null;
   
};
/**
 * Disallow decoration.
 * @override
 */
ble.scribble.style.StylePicker.prototype.canDecorate = function(element) {
  return false;
};

ble.scribble.style.StylePicker.prototype.getFilled = function() {
  return false;
};

ble.scribble.style.StylePicker.prototype.getSelectedMethod = function() {
  return 0;
};

ble.scribble.style.StylePicker.prototype.getStrokeColor = function() { 
  return goog.color.alpha.hexToRgbaStyle(this.hsva1.getColorRgbaHex());
};

ble.scribble.style.StylePicker.prototype.getFillColor = function() { 
  return goog.color.alpha.hexToRgbaStyle(this.hsva2.getColorRgbaHex());
};

ble.scribble.style.StylePicker.prototype.getStyle = function() {
  var lineWidth = this.slider.getValue();
  if(this.getFilled()) {
    return new ble.gfx.path.PainterPixel(lineWidth, this.getStrokeColor(), this.getFillColor());
  } else {
    return new ble.gfx.path.PainterPixel(lineWidth, this.getStrokeColor()); 
  }
};

ble.scribble.style.StylePicker.prototype.createDom = function() {
  var domHelper = this.getDomHelper();
  var container = domHelper.createDom('div', {'class': 'ble-scribble-stylepicker'});
  this.setElementInternal(container);

  this.bigIcon = new ble.scratch.Canvas(this.height, this.height);
  this.addChild(this.bigIcon, true);

  var smallIconContainer = new goog.ui.Component();
  var height = this.height;
  smallIconContainer.createDom = function() {
    var domHelper = this.getDomHelper();
    var elt = domHelper.createElement("div");
    elt.style['width'] = height; 
    elt.style['height'] = height; 
    elt.style['display'] = "inline-block";
    this.setElementInternal(elt);
  };
  //replace manual styling with css class plz
  this.smallIcons = [];
  for(var i = 0; i < 4; i++) {
    var smallIcon = new ble.scratch.Canvas(this.smallSize, this.smallSize);
    smallIconContainer.addChild(smallIcon, true);
    this.smallIcons.push(smallIcon);
  };
  this.smallIconContainer = smallIconContainer;
  this.addChild(smallIconContainer, true);

  this.slider = new goog.ui.Slider();
  this.slider.setOrientation(goog.ui.Slider.Orientation.VERTICAL);
  this.addChild(this.slider, true);


  this.hsva1 = new goog.ui.HsvaPalette(domHelper, '#000', 1.0, 'goog-hsva-palette-sm');
  this.addChild(this.hsva1, true);

  this.hsva2 = new goog.ui.HsvaPalette(domHelper, '#ffc', 1.0, 'goog-hsva-palette-sm'); 
  this.addChild(this.hsva2, true);
};

ble.scribble.style.StylePicker.prototype.enterDocument = function() {
  goog.base(this, "enterDocument");

  var sEl = this.slider.getElement();
  sEl.style['width'] = 20;
  sEl.style['height']  = this.height;
  sEl.style['display'] = "inline-block";
  this.slider.setStep(1);
  this.slider.setMinimum(1);
  this.slider.setMaximum(75);
  this.slider.setValue(1);

  this.bigIcon.getRawContext().lineJoin = "round";
  this.bigIcon.getRawContext().lineCap = "round";

  goog.events.listen(this.slider.getElement(), goog.events.EventType.MOUSEUP, this.animateBig, false, this);
  goog.events.listen(this.slider, goog.ui.Component.EventType.CHANGE, this.repaintAll, false, this);
  goog.events.listen(this.hsva1, goog.ui.Component.EventType.ACTION, this.repaintAll, false, this);
  goog.events.listen(this.hsva2, goog.ui.Component.EventType.ACTION, this.repaintAll, false, this);
  this.repaintAll();
};

/**
 * @param{number} scale
 * @param{Array.<number>} coordinates
 * @return{Array.<number>}
 */
ble.scribble.icon.scaleUp = function(scale, coordinates) {
  var result = coordinates.slice();
  for(var i = 0; i < result.length; i++) {
    result[i] = Math.round(result[i] * scale);
  }
  return result;
};

ble.scribble.icon.flattenTime = function(time, factor) {
  var L = time.length - 1;
  var time0 = time[0];
  var timeF = time[time.length - 1];
  var interval = timeF - time0;

  var delta = time.slice();
  for(var i = 0; i <= L; i++) {
    delta[i] = (delta[i] - time0) / interval;
    delta[i] = factor * delta[i] + (1.0 - factor) * (i / L);
    delta[i] = time0 + interval * delta[i];
  }
  return delta;
};

goog.exportSymbol('ble.scribble.style.StylePicker', ble.scribble.style.StylePicker);
goog.exportProperty(ble.scribble.style.StylePicker.prototype, 'render', ble.scribble.style.StylePicker.prototype.render);
