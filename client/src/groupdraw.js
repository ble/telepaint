goog.require('goog.events.EventHandler');
goog.require('goog.ui.Component');

goog.require('ble.scribble.UI');
goog.require('ble.scribble.Canvas');

goog.require('ble.rpc.EventType');

goog.provide('ble.game.GroupDraw');

/*
A game needs to be its own island of functionality, having a model,
view (in the form of DOM elements), and controller (in the form of
event handlers and targets).

The GroupDraw must handle at least 3 kinds of events:
1) User draws something (ble.scribble.Canvas.EventType.END)
2) Server acks sent draw part (ble.rpc.CALL_SUCCESS)
3) Server reports that something was drawn by a peer (ble.rpc.RESPONSE)

For ease of prototyping, it will be a Component for right now.
*/

////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
////////////////////////////////////////////////////////////////////////////////

var canvasType = ble.scribble.Canvas.EventType;
var rpcType = ble.rpc.EventType;


/** @constructor
 * @extends {goog.ui.Component}
 * @param {number} width
 * @param {number} height
 */
ble.game.GroupDraw = function(width, height) {
  goog.ui.Component.call(this);
  this.UI = new ble.scribble.UI(width, height);
  this.addChild(this.UI, true);
  this.handler = new goog.events.EventHandler(this);
};
goog.inherits(ble.game.GroupDraw, goog.ui.Component);

/**
 * @param {ble.room.Connection?} connection
 */
ble.game.GroupDraw.prototype.bindConnection = function(connection) {
  if(goog.isDefAndNotNull(this.connection))
    throw new Error('');
  this.connection = connection;
  this.handler.listen(this.connection, rpcType.RESPONSE, this.handleReceived);
};

ble.game.GroupDraw.prototype.enterDocument = function() {
  goog.base(this, 'enterDocument');
  this.canvas = this.UI.canvas;
  this.drawing = this.UI.canvas.drawing;

  this.handler.listen(this.canvas, canvasType.END, this.handleDraw); 
};

ble.game.GroupDraw.prototype.handleDraw = function(event) {
  var canvas = event.target;
  var drawing = canvas.drawing;
  var drawPart = drawing.getCurrent();
  drawing.setCurrent(null);
  canvas.withContext(canvas.repaintComplete);
  window.setTimeout(
      goog.bind(drawing.addAtEnd, drawing, drawPart),
      1000);
  window.setTimeout(
      goog.bind(canvas.withContext, canvas, canvas.repaintComplete),
      1500);
  return false; //don't allow the default action (append to drawing)
};


////////////////////////////////////////////////////////////////////////////////
                                                                           });
////////////////////////////////////////////////////////////////////////////////
