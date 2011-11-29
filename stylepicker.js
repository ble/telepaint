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
goog.require('ble.json.PrettyPrinter');
goog.require('ble.scribble.style.IconPainter');

goog.provide('ble.scribble.backdropOn');

goog.provide('ble.scribble.style.StylePicker');
goog.provide('ble.scribble.style.EventType');

/**
 * @enum{string}
 */
ble.scribble.style.EventType = {
  METHODCHANGED: 'methodchange',
  STYLECHANGED: 'stylechange'
};

/**
 * @constructor
 * @extends {goog.ui.Component}
 */
ble.scribble.style.StylePicker = function() {
  goog.ui.Component.call(this);
  this.smallSize = Math.floor(this.height / 2);
  this.selectedMethod = 0;
  this.painter = new ble.scribble.style.IconPainter(this);
};
goog.inherits(ble.scribble.style.StylePicker, goog.ui.Component);


ble.scribble.style.StylePicker.prototype.height = 152;
/*
ble.scribble.style.StylePicker.prototype.animateBig = function() {
  if(this.animationHandle_ != null)
    this.cancelAnimation();
  this.animationStart = Date.now();
  this.animationDuration = 2500;
  this.animationFn = goog.bind(function() {
    var now = Date.now();  
    var delta = (now - this.animationStart) / this.animationDuration;
    if(delta >= 1) {
      this.cancelAnimation();
      this.repaintAll();
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
};

ble.scribble.style.StylePicker.prototype.cancelAnimation = function() {
  if(this.animationHandle_ == null)
    return;
  window.clearInterval(this.animationHandle_);
  this.animationHandle_ = null;
   
};
*/
/**
 * Disallow decoration.
 * @override
 */
ble.scribble.style.StylePicker.prototype.canDecorate = function(element) {
  return false;
};

ble.scribble.style.StylePicker.prototype.getFilled = function() {
  return (this.getSelectedMethod() == 3);
};

ble.scribble.style.StylePicker.prototype.getSelectedMethod = function() {
  return this.selectedMethod;
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
    return new ble._2d.path.PainterVirtual(lineWidth, this.getStrokeColor(), this.getFillColor());
  } else {
    return new ble._2d.path.PainterVirtual(lineWidth, this.getStrokeColor()); 
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

  for(var i = 0; i < this.smallIcons.length; i++) {
    var ctx = this.smallIcons[i].getRawContext();
    ctx.lineJoin = "round";
    ctx.lineCap = "round"; 
  }
  goog.events.listen(
    this.smallIconContainer.getElement(),
    goog.events.EventType.CLICK,
    function(event){
      var changed = false;
      for(var i = 0; i < this.smallIcons.length; i++) {
        if(event.target === this.smallIcons[i].getElement()) {
          this.selectedMethod = i;
          changed = true;
        }
      }
      if(changed) {
        this.repaint();
        this.methodChanged();
      };
    },
    false,
    this);

  var styleChanged = function(event) {
    this.repaint();
    this.styleChanged();
  };

  goog.events.listen(
    this.slider,
    goog.ui.Component.EventType.CHANGE,
    styleChanged,
    false,
    this);
  goog.events.listen(
    this.hsva1,
    goog.ui.Component.EventType.ACTION,
    styleChanged,
    false,
    this);
  goog.events.listen(
    this.hsva2,
    goog.ui.Component.EventType.ACTION,
    styleChanged,
    false,
    this);
  this.repaint();
  this.styleChanged();
  this.methodChanged();
};

ble.scribble.style.StylePicker.prototype.repaint = function() {
  this.painter.paintIcons();
  this.painter.paintPreview();
};

ble.scribble.style.StylePicker.prototype.styleChanged = function() {
  var e = new goog.events.Event(ble.scribble.style.EventType.STYLECHANGED);
  e.method = this.getSelectedMethod();
  e.style = this.getStyle();
  this.dispatchEvent(e);
};

ble.scribble.style.StylePicker.prototype.methodChanged = function() {
  var e = new goog.events.Event(ble.scribble.style.EventType.METHODCHANGED);
  e.method = this.getSelectedMethod();
  e.style = this.getStyle();
  this.dispatchEvent(e);
};

goog.exportSymbol('ble.scribble.style.StylePicker', ble.scribble.style.StylePicker);
goog.exportProperty(ble.scribble.style.StylePicker.prototype, 'render', ble.scribble.style.StylePicker.prototype.render);
