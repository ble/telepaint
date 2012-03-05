goog.require('goog.ui.Component');

goog.require('ble.room.Model');
goog.require('ble.room.Observer');

goog.provide('ble.room.Dom');
////////////////////////////////////////////////////////////////////////////////
                                                        goog.scope(function(){
//////////////////////////////////////////////////////////////////////////////// 
/**
 * @constructor
 * @extends {goog.ui.Component}
 */
ble.room.Dom = function() {
  goog.ui.Component.call(this);
};
goog.inherits(ble.room.Dom, goog.ui.Component);

ble.room.Dom.EventType = ({
  CHAT: 'CHAT'
});

var dp = ble.room.Dom.prototype;
dp.createDom = function() {
  var dom = this.dom_;
  var name = dom.createDom('div', {'class': 'room-name'}, 'room name here');

  var chatSpace = dom.createDom('div', {'class': 'chat-space'});
  var chatInput = dom.createDom('input', {'type': 'text', 'class': 'chat-input'}); 
  var chatForm = dom.createDom('form', null, chatInput);
  var chatPane = dom.createDom('div', {'class': 'chat-pane'}, chatSpace, chatForm);

  var observerContainer = dom.createDom('div', {'class': 'room-observers'});
  var container = dom.createDom('div', {'class': 'room-container'}, name, observerContainer, chatPane);
  this.name = name;
  this.observerContainer = observerContainer;
  this.container = container;

  this.chatForm = chatForm;
  this.chatInput = chatInput;
  this.chatSpace = chatSpace;

  this.setElementInternal(container);
};

dp.enterDocument = function() {
  goog.base(this, 'enterDocument');
  goog.events.listen(this.chatForm, goog.events.EventType.SUBMIT, this);
};

dp.handleEvent = function(event) {
  if(event.type == goog.events.EventType.SUBMIT) {
    event.preventDefault();
    var chatEvent = new goog.events.Event(ble.room.Dom.EventType.CHAT);
    chatEvent.msg = this.chatInput.value;
    this.dispatchEvent(chatEvent);
    this.chatInput.value = "";
  }
};

/**
 * @param {ble.room.Model} model
 */
dp.set = function(model) {
  var dom = this.dom_;
  dom.removeChildren(this.name);
  dom.append(this.name, model.name);

  dom.removeChildren(this.observerContainer);
  dom.append(this.observerContainer, goog.array.map(model.observers, this.makeObserverDom, this));
};

dp.setGame = function(component) {
  this.addChild(component, true);
};

/**
 * @param {ble.room.Observer} observer
 */
dp.makeObserverDom = function(observer) {
  var dom = this.dom_;
  var clss = observer.self ? 'observer-other' : 'observer-self';
  var anonymous = !goog.isDefAndNotNull(observer.name);
  clss = anonymous ? clss + " observer-anonymous" : clss;
  var nameText = !anonymous ?
    "< " + observer.name + " >" :
    "{ anonymous observer }";
  return dom.createDom('div',
      null,
      dom.createDom('a',
        {'title': observer.id,
         'class': clss}, nameText));
};

dp.chat = function(observer, message) {
  var dom = this.dom_;
  var text = dom.createTextNode(message);
  var obsDom = this.makeObserverDom(observer);
  var container = dom.createDom('div', {'class': 'chat-message'}, obsDom, text);
  this.chatSpace.appendChild(container);
};
////////////////////////////////////////////////////////////////////////////////
                                                                             });
//////////////////////////////////////////////////////////////////////////////// 


