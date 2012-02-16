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

var dp = ble.room.Dom.prototype;
dp.createDom = function() {
  var dom = this.dom_;
  var name = dom.createDom('div', {'class': 'room-name'}, 'room name here');
  var observerContainer = dom.createDom('div', {'class': 'room-observers'});
  var container = dom.createDom('div', {'class': 'room-container'}, name, observerContainer);
  this.name = name;
  this.observerContainer = observerContainer;
  this.container = container;
  this.setElementInternal(container);
};

dp.enterDocument = function() {
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

/**
 * @param {ble.room.Observer} observer
 */
dp.makeObserverDom = function(observer) {
  var dom = this.dom_;
  var clss = observer.self ? 'observer-other' : 'observer-self';
  return dom.createDom('div',
      null,
      dom.createDom('a',
        {'title': observer.id,
         'class': clss}, observer.name));
};
////////////////////////////////////////////////////////////////////////////////
                                                                             });
//////////////////////////////////////////////////////////////////////////////// 


