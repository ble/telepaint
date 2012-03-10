goog.require('ble.icon');

goog.require('goog.dom.DomHelper');
goog.require('goog.ui.HsvaPalette');
goog.require('goog.events');
goog.require('goog.ui.Component.EventType');
goog.require('goog.ui.Slider');

goog.provide('ble.icon.run_test');

ble.icon.run_test = function() { 
  var domHelper = new goog.dom.DomHelper();
  var container = domHelper.getElement("outermost");

  var style = new ble.icon.Style("#2a4", "#fff", 5);
  var icons = new ble.icon.Icons(style);
  var toDraw = [icons.stroke, icons.polyline, icons.polylineFill, icons.erase];
  var methods = ["stroke", "polyline", "polylineFill", "erase"];
  var selected = 0;
  style.method = methods[selected];

  var allControls = domHelper.createDom("div");
  container.appendChild(allControls);
  var allIcons = domHelper.createDom("div"); {
    var bigIcon = icons.addIcon(allIcons, 120, goog.bind(toDraw[selected], icons));

    var updateBig = function(selection) {
      selected = selection;
      style.method = methods[selected];
      icons.setIcon(0, goog.bind(toDraw[selected], icons));
      icons.redraw();
    }

    for(var i = 0; i < toDraw.length; i++) {
      var painter = toDraw[i];
      var icon = icons.addIcon(allIcons, 40, goog.bind(painter, icons)); 
      (function() {
        var ii = i;
        goog.events.listen(icon.getElement(), goog.events.EventType.CLICK, function() {
          updateBig(ii);
        });
      })();
    }
    allControls.appendChild(allIcons);
  }

  var p1C = domHelper.createDom("div", {style: "position:absolute; display: inline;"});
  var palette1 = new goog.ui.HsvaPalette(null, style.color1, 1, "goog-hsva-palette-sm");
  goog.events.listen(palette1, goog.ui.Component.EventType.ACTION, function() {
    style.color1 = goog.color.alpha.hexToRgbaStyle(palette1.getColorRgbaHex());
    icons.redraw();
  });
  palette1.render(p1C);
  allControls.appendChild(p1C);
  

  var p2C = domHelper.createDom("div", {style: "left: 210px; position: absolute; display: inline;"});
  var palette2 = new goog.ui.HsvaPalette(null, style.color2, 1, "goog-hsva-palette-sm");
  goog.events.listen(palette2, goog.ui.Component.EventType.ACTION, function() {
    style.color2 = goog.color.alpha.hexToRgbaStyle(palette2.getColorRgbaHex());
    icons.redraw();
  });

  palette2.render(p2C);
  allControls.appendChild(p2C);

  var sliderC = domHelper.createDom("div", {style: "left: 420px; position: relative;"});
  var slider = new goog.ui.Slider();
  slider.setOrientation(goog.ui.Slider.Orientation.VERTICAL);
  slider.setMinimum(1);
  slider.setMaximum(100);

  slider.render(sliderC);
  allControls.appendChild(sliderC);
  slider.setValue(3);

  goog.events.listen(slider, goog.ui.Component.EventType.CHANGE, function(e) {
    style.strokeWidth = slider.getValue();
    icons.redraw();
  });
};
