goog.require('goog.dom.DomHelper');
goog.require('goog.ui.Component');
goog.require('goog.events');
goog.require('goog.events.EventType');
goog.require('goog.color');


goog.provide('ble.tpaint.TableUI');
goog.provide('ble.tpaint.testTable');
goog.provide('ble.tpaint.colorWheel');


ble.tpaint.colorWheel = function(radians, lum) {
  if(lum === undefined)
    lum = 1.0;
  while(radians > 2 * Math.PI) {
    radians -= 2 * Math.PI;
  }
  while(radians < 0) {
    radians += 2 * Math.PI;
  }
  var ixMajor = Math.floor(radians / (2 * Math.PI / 3)) % 3;
  var ixMinor = (8 - Math.floor(radians / (Math.PI / 3))) % 3;
  var center = (1 + 2 * ixMajor) * Math.PI / 3;
  var minor = 1.5 * Math.abs(radians - center);
  var magMajor = Math.floor(lum * 255);
  var magMinor = Math.floor(lum * 255 * Math.sin(minor));
  var red = 0, green = 0, blue = 0;
  console.log([radians, minor]);
  console.log([ixMajor, ixMinor]);
  switch(ixMajor) {
    case 0: red = magMajor; break;
    case 1: green = magMajor; break;
    case 2: blue = magMajor; break;
  }
  switch(ixMinor) {
    case 0: red = magMinor; break;
    case 1: green = magMinor; break;
    case 2: blue = magMinor; break; 
  }
  console.log([red, green, blue]);
  return goog.color.rgbToHex(red, green, blue);
}

ble.tpaint.testNames = [
  'majestic mapp',
  'trumcapz',
  'w/ospaces',
  'brosophone',
  'brosephone',
  'scientific mapp',
  'sir cloudsley shovel',
  'gaston j. feeblebunny'];

(function() {
  ble.tpaint.testTable = [];
  var deltaAngle = Math.PI * 2 / 5; 
  var luminances = [1.0, 0.5, 0.7];
  for(var i = 0; i < ble.tpaint.testNames.length; i++) {
    var color = ble.tpaint.colorWheel(deltaAngle * i, luminances[i % luminances.length]);
    ble.tpaint.testTable.push({
      'name': ble.tpaint.testNames[i],
      'color': color});
  }
})();

/**
 * @constructor
 * @extends {goog.ui.Component}
 */

ble.tpaint.TableUI = function(players, fContainer) {
  goog.ui.Component.call(this);
  this.players = players;
  this.fContainer = fContainer;

}
goog.inherits(ble.tpaint.TableUI, goog.ui.Component);


ble.tpaint.TableUI.prototype.rotateRows = function() {
  var L = this.rows_.length;
  var removed = this.dom_.removeNode(this.rows_[L-1].element);
  this.dom_.insertSiblingBefore(removed, this.rows_[0].element);
  this.rows_.unshift(this.rows_.pop());
}

ble.tpaint.TableUI.prototype.passStack = function(index) {
  var L = this.rows_.length;
  var from = this.rows_[index];
  var to = this.rows_[(index + 1) % L];
  
  if(from.stacks.length > 0) {
    var removed = this.dom_.removeNode(from.stacks[0]);
    this.dom_.appendChild(to.element, removed);
    to.stacks.push(from.stacks.shift());
    removed.style['font-weight'] = "";

    if(/writing/.test(removed.innerHTML)) {
      removed.innerHTML = "&nbsp;drawing";
    } else {
      removed.innerHTML = "&nbsp;writing";
    }

    if(to.stacks.length > 0) {
      to.stacks[0].style['font-weight'] = "bold";
    }
    if(from.stacks.length > 0) {
      from.stacks[0].style['font-weight'] = "bold";
    }
  }
}

ble.tpaint.TableUI.prototype.createDom = function() {
  this.element_ = this.dom_.createElement('div');
  this.rows_ = [];
  for(var i = 0; i < this.players.length; i++) {
    var player = this.players[i];
    var playerLabel = this.dom_.createElement('div');
    playerLabel.innerHTML = player.name;
    playerLabel.style.color = player.color;
    playerLabel.style.display = "table-cell";

    var stackLabel = this.dom_.createElement('div');
    stackLabel.innerHTML = "&nbsp;writing";
    stackLabel.style.color = player.color;
    stackLabel.style.display = "table-cell";
    stackLabel.style['font-weight'] = "bold";

    var rowElement = this.dom_.createDom("div", null, [playerLabel, stackLabel]);
    rowElement.style.display = "table-row";
    var row = {
      'element': rowElement,
      'player': playerLabel,
      'stacks': [stackLabel]
    };
    this.rows_.push(row);
    this.dom_.appendChild(this.element_, rowElement); 
  }
};

