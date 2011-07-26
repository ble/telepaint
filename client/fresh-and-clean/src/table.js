goog.require('goog.dom.DomHelper');
goog.require('goog.ui.Component');
goog.require('goog.events');
goog.require('goog.events.EventType');


goog.provide('ble.tpaint.TableUI');
goog.provide('ble.tpaint.testTable');


ble.tpaint.testTable = [
  {'name': 'majestik bouftwo',
   'color': '#8a8'},
  {'name': 'implements cumtrapz',
   'color': '#c0c'},
  {'name': 'wizard without spaces',
   'color': '#f08'}];



/**
 * @constructor
 * @extends {goog.ui.Component}
 */

ble.tpaint.TableUI = function(players) {
  goog.ui.Component.call(this);
  this.players = players;

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

