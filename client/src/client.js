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

goog.require('ble.game.GroupDraw');

goog.require('goog.debug.ErrorHandler');

goog.provide('ble.room.Client');

////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
////////////////////////////////////////////////////////////////////////////////

var console = window.console;
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
  this.game = null;
  goog.events.listen(
      this.connection,
      [ble.rpc.EventType.RESPONSE],
      this);
  goog.events.listen(
      this.dom,
      [ble.room.Dom.EventType.CHAT],
      this);
};
goog.inherits(ble.room.Client, goog.events.EventTarget);


var cp = ble.room.Client.prototype;

cp.gameRegistry = (function() {
  var games = [ble.game.GroupDraw];
  var result = {};
  for(var i = 0; i < games.length; i++) {
    var game = games[i];
    result[game.methodPrefix] = game;
  };
  return result;
})();

cp.startGame = function(game) {
  this.game = game;
  this.gamePrefix = game.methodPrefix;
  this.dom.setGame(game);
};

cp.handleEvent = function(event) {
  switch(event.type) {
    case ble.rpc.EventType.RESPONSE:
      this.handleMethod(event.result.method, event.result);
      break;

    case ble.room.Dom.EventType.CHAT:
      var rpc = new ble.json.RpcCall(
          'chat',
          {'message': event.msg});
      this.connection.postRoom(rpc);
      break;

    default:
//      console.log("Unhandled event of type " + event.type);
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
  var parts = method.split(/:/);
  var call = ble.json.RpcCall.coerce(obj);
  //var method = call.method;
  var params = call.params; 
  if(parts.length == 1) {
    switch(method) {
      case 'set_name':
        var who = params['who'], name = params['name'];
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
        var who = params['who'], name = params['name'];
        if(!name) name = null;
        var joined = new ble.room.Observer(name, who);
        this.state.addObserver(joined);
        this.updateState(this.state);
        break;

      case 'chat':
        var who = params['who'], message = params['message'];
        this.dom.chat(this.state.getObserver(who), message);
        break;

      default:
//        console.error('Unknown method: ' + method);
//        console.error(obj);
    }
  } else if(parts.length == 2) {
    var gameName = parts[0];
    var gameMethod = parts[1];
    if(gameMethod == 'init') {
      var constructor = this.gameRegistry[gameName];
      var game = constructor.varArgs(params);
      game.bindToClient(this);
      this.startGame(game);
    }
  } else {
//    console.error('Unknown method: ' + method);
//    console.error(obj);
  }
};

cp.pickName = function(nameString) {
  var acceptable = /^\s*([a-zA-Z0-9_][!-~]+)\s*$/;
  var match = nameString.match(acceptable);
  if(!match) {
    var dlg = this.dlg;
    var showAgain = goog.bind(dlg.setVisible, dlg, true);
    window.setTimeout(showAgain, 0);
    return;
  }
  var rpc = new ble.json.RpcCall(
      'set_name',
      {'who': this.state.obsSelf.id,
       'name': match[1]});
  goog.events.listenOnce(
      rpc,
      ble.rpc.EventType.ALL,
      this.handleSetNameResponse,
      false,
      this);
  this.connection.postRoom(rpc);
};

cp.handleSetNameResponse = function(event) {
  if(event.type == ble.rpc.EventType.CALL_ERROR) { 
    this.promptForName('Server says: "' + event.error.message + '"');
  } else if(event.type == ble.rpc.EventType.TRANSPORT_ERROR ||
            event.type == ble.rpc.EventType.FORMAT_ERROR) {
    this.promptForName('The server might be having problems...');
  }
};

cp.setupLinks = function() {
  var roomUri = '/room/' + this.roomId;
  var gameUri = roomUri + '/game';
  var queueUri = roomUri + '/queue/' + this.observerId;
  var obj = (
      {'room': roomUri,
       'queue': queueUri,
       'game': gameUri});
  ble.hate.addLinks(obj);
};

cp.fetchState = function() {
  this.connection.fetchState();
};


////////////////////////////////////////////////////////////////////////////////
                                                                           });
////////////////////////////////////////////////////////////////////////////////
var errorHandler = new goog.debug.ErrorHandler(function(e) {
//  console.log('Intercepted error:');
//  console.error(e);
  window.lastError = e;
  throw e;
});
var dom = new ble.room.Dom();
dom.render(document.body);

goog.net.XhrIo.protectEntryPoints(errorHandler);
var client = new ble.room.Client(dom);
client.setupLinks();
client.fetchState();


