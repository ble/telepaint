goog.require('ble.SnowflakeApp');

var domHelper = new goog.dom.DomHelper();
var container = domHelper.getElement("outermost");
var app = new ble.SnowflakeApp(container);
app.run();

