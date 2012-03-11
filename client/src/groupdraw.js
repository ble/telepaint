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
1) User draws something (ble.scribble.Canvas.EventType.END on canvas)
2) Server acks sent draw part (ble.rpc.CALL_SUCCESS on submitted RPC)
3) Server reports that something was drawn by a peer (ble.rpc.RESPONSE on queue)

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
  this.pendingDraw = {};
};
goog.inherits(ble.game.GroupDraw, goog.ui.Component);

ble.game.GroupDraw.varArgs = function(params) { 
  var size = params['size'];
  if(size.length != 2)
    throw new Error();
  return new ble.game.GroupDraw(size[0], size[1]);
};

var GDp = ble.game.GroupDraw.prototype;

ble.game.GroupDraw.methodPrefix = "group_draw";
GDp.methodPrefix = ble.game.GroupDraw.methodPrefix;
GDp.deserializer = ble.scribbleDeserializer;

GDp.dispose = function() {
  this.handler.dispose();
  goog.base(this, 'dispose');
};

GDp.bindToClient = function(client) {
  this.client = client;
  this.bindConnection(client.connection);
};
/**
 * @param {ble.room.Connection?} connection
 */
GDp.bindConnection = function(connection) {
  if(goog.isDefAndNotNull(this.connection))
    throw new Error('');
  this.connection = connection;
  this.handler.listen(this.connection, rpcType.RESPONSE, this.handleReceived);
};

GDp.enterDocument = function() {
  goog.base(this, 'enterDocument');
  this.canvas = this.UI.canvas;
  this.drawing = this.UI.canvas.drawing;

  this.handler.listen(this.canvas, canvasType.END, this.handleDraw); 
};

GDp.handleDraw = function(event) {
  var canvas = event.target;
  var drawing = canvas.drawing;
  var drawPart = drawing.getCurrent();
  drawing.setCurrent(null);
  canvas.withContext(canvas.repaintComplete);
  var rpc = new ble.json.RpcCall(
      'draw',
      {'who': this.client.state.obsSelf.id,
       'what': drawPart});
  rpc.id = ble.rpc.id();
  this.pendingDraw[rpc.id] = {
    drawPart: drawPart,
    drawing: drawing,
    canvas: canvas,
    drawn: false,
    received: false
  },
  this.handler.listenOnce(
      rpc,
      rpcType.ALL,
      this.handleDrawResponse);
  this.connection.postGame(rpc);
  return false;
};

GDp.succeedDraw = function(id) {
  var pend = this.pendingDraw[id];
  //TODO: fix so that we use the server timestamped drawpart...
  pend.drawing.add(pend.drawPart);
  pend.canvas.withContext(pend.canvas.repaintComplete);
  pend.drawn = true; 
  if(pend.drawn && pend.received)
    delete this.pendingDraw[id];
};

GDp.receiveDraw = function(id) {
  var pend = this.pendingDraw[id];
  if(goog.isDefAndNotNull(pend)) {
    pend.received = true;
    if(pend.drawn && pend.received)
      delete this.pendingDraw[id];
    return false;
  }
  return true;
};

GDp.handleDrawResponse = function(event) {
  var rpc = event.target;
  switch(event.type) {
    case rpcType.CALL_SUCCESS:
      this.succeedDraw(rpc.id);
      break;
    default:
      console.log(event.type);
  }
};

GDp.handleReceived = function(event) {
  var method = event.method;
  switch(method) {
    case 'draw':
      var drawCall = ble.json.RpcCall.coerce(event.result);
      var params = drawCall.params;
      var id = drawCall.id;
      var drawPart = this.deserializer.deserialize(params['what']);
      if(this.receiveDraw(id)) {
        this.drawing.add(drawPart);
        this.canvas.withContext(this.canvas.repaintComplete);
        window.setTimeout(goog.bind(this.canvas.replayAll, this.canvas, 1000), 10);
      }
  };
};

////////////////////////////////////////////////////////////////////////////////
                                                                           });
////////////////////////////////////////////////////////////////////////////////
