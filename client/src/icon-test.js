goog.require('ble.icon');

goog.require('goog.dom.DomHelper');

goog.provide('ble.icon.run_test');

ble.icon.run_test = function() { 
  var domHelper = new goog.dom.DomHelper();
  var container = domHelper.getElement("outermost");

  ble.icon.render(container, ble.icon.stroke);
  ble.icon.render(container, ble.icon.polyline);
  ble.icon.render(container, ble.icon.polylineFill);
  ble.icon.render(container, ble.icon.erase);
};
