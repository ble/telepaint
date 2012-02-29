goog.require('goog.events.EventTarget');
goog.require('goog.events');
goog.require('goog.net.Cookies');
goog.require('goog.net.XhrIo');

goog.require('goog.ui.Prompt');

goog.require('ble.json.RpcCall');
goog.require('ble.hate');
goog.require('ble.erlToDate');

goog.require('ble.room.Connection');

goog.require('ble.room.Observer');
goog.require('ble.room.Model');
goog.require('ble.room.Dom');

goog.require('goog.debug.ErrorHandler');

goog.provide('ble.room.Client');
var console = window.console;

////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
////////////////////////////////////////////////////////////////////////////////

var Observer = ble.room.Observer;
var Model = ble.room.Model;
/**
 * @enum{string}
 */
ble.room.EventType = ({
  FETCHED_STATE: 'FETCHED_STATE',
  UPDATE: 'UPDATE',
  DISCONNECTED: 'DISCONNECTED'
});
var EventType = ble.room.EventType;

/**
 * @constructor
 * @param {ble.room.Dom} dom
 * @extends {goog.events.EventTarget}
 */ 
ble.room.Client = function(dom) {
  goog.events.EventTarget.call(this);
  this.dom = dom;
  var pattern = /^http:\/\/([^\/]*)\/room\/([0-9a-zA-Z_-]+)\/client(?:\?join)?$/;
  var match = window.location.href.match(pattern);
  if(!match)
    throw new Error('unexpected location');
  this.host = match[1];
  this.roomId = match[2];
  var c = new goog.net.Cookies(document);
  this.observerId = c.get('observerId');
  this.state = null;
  this.lastUpdated = 0;
  this.connection = new ble.room.Connection();
  goog.events.listen(
      this.connection,
      [ble.rpc.EventTypes.RESPONSE],
      this);
  goog.events.listen(
      this.dom,
      [ble.room.Dom.EventType.CHAT],
      this);
};
goog.inherits(ble.room.Client, goog.events.EventTarget);


var cp = ble.room.Client.prototype;

cp.handleEvent = function(event) {
  console.log('Client.handleEvent called');
  console.log(event);
  switch(event.type) {
    case ble.rpc.EventTypes.RESPONSE:
      this.handleMethod(event.result.method, event.result);
      break;

    case ble.rpc.EventTypes.ERROR:
      this.handleFailedRpc(event);
      break;

    case ble.room.Dom.EventType.CHAT:
      var rpc = new ble.json.RpcCall(
          'chat',
          {'message': event.msg});
      this.connection.postRpc(rpc);
      break;

    default:
      console.log("Unhandled event of type " + event.type);
  }
};

cp.handleFailedRpc = function(event) {
  switch(event.method) {
    case 'set_name':
      this.promptForName('Server says: "' + event.error.message + '"');
      break;
    default:
      console.error('RPC ' + event.method + ' failed.');
  }
};

/**
 * @param {string=} message
 */
cp.promptForName = function(message) {
  message = goog.isDefAndNotNull(message) ? message : 'Pick a name that others will see.'; 
  if(!goog.isDefAndNotNull(this.state.myName())) {
     this.dlg = new goog.ui.Prompt(
         'Choose your handle',
         message,
         goog.bind(this.pickName, this),
         'modal-dialog');
     this.dlg.setVisible(true);
  } 
};

cp.updateState = function(state) {
  this.state = state;
  this.dom.set(this.state);
  this.promptForName();
};

cp.handleMethod = function(method, obj) {
  switch(method) {
    case 'set_name':
      var who = obj['who'], name = obj['name'];
      this.state.byId[who].name = name;
      this.dom.set(this.state);
      break;

    case 'room_state':
      var rO = obj['observers'];
      var observers = [];
      var self = null;
      for(var i = 0; i < rO.length; i++) {
        var obs = new Observer(rO[i]['name'], rO[i]['id']);
        if(rO[i]['self'])
          self = obs;
        observers.push(obs);
      }
      var model = new ble.room.Model(obj['name'], observers, self);
      this.updateState(model);
      this.connection.pollCometFrom(obj['when']);
      break;

    case 'join_room':
      var who = obj['who'], name = obj['name'];
      if(!name) name = null;
      var joined = new ble.room.Observer(name, who);
      this.state.addObserver(joined);
      this.updateState(this.state);
      break;

    case 'chat':
      var who = obj['who'], message = obj['message'];
      this.dom.chat(this.state.getObserver(who), message);
      break;

    default:
      console.error('Unknown method: ' + method);
      console.error(obj);
  }
};

cp.pickName = function(nameString) {
  var acceptable = /\s*([a-zA-Z0-9_][!-~]+)\s*/;
  var match = nameString.match(acceptable);
  if(!match) {
    var dlg = this.dlg;
    var showAgain = goog.bind(dlg.setVisible, dlg, true);
    window.setTimeout(showAgain, 0);
    return;
  }
  console.log(match[1]);

  this.connection.sendSetName(
    this.state.obsSelf.id,
    match[1]);
};

cp.setupLinks = function() {
  var roomUri = '/room/' + this.roomId;
  var queueUri = roomUri + '/queue/' + this.observerId;
  var obj = (
      {'room': roomUri,
       'queue': queueUri});
  ble.hate.addLinks(obj);
};

cp.fetchState = function() {
  this.connection.fetchState();
};


////////////////////////////////////////////////////////////////////////////////
                                                                           });
////////////////////////////////////////////////////////////////////////////////
var errorHandler = new goog.debug.ErrorHandler(function(e) {
  console.log('Intercepted error:');
  console.error(e);
  window.lastError = e;
  throw e;
});
var dom = new ble.room.Dom();
dom.render(document.body);

goog.net.XhrIo.protectEntryPoints(errorHandler);
var client = new ble.room.Client(dom);
client.setupLinks();
client.fetchState();


