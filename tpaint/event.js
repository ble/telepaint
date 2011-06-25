goog.require('goog.events.Event');
goog.provide('ble.tpaint.EventTypes');
goog.provide('ble.tpaint.StateEvent');
goog.provide('ble.tpaint.UserID');

/**
 * Constants for client events
 * @enum{string}
 */
ble.tpaint.EventTypes = {
  'ENTER_LOBBY': 'ENTER_LOBBY',
  'PICK_PSEUDONYM': 'PICK_PSEUDONYM',
  'JOIN_PREGAME': 'JOIN_PREGAME',
  'CHAT': 'CHAT',
  'START_GAME': 'START_GAME',
  'STROKE': 'STROKE',
  'PASS_DRAWING': 'PASS_DRAWING',
  'RECEIVE_DRAWING': 'RECEIVE_DRAWING',
  'PEEK': 'PEEK',
  'WALLHAX': 'WALLHAX',
  'COMPLETE': 'COMPLETE',
  'OBSERVE': 'OBSERVE'
};


/**
 * A state transition event.
 * @constructor
 * @param {string} type
 * @param {string} who
 * @param {?string} fromState
 * @param {string} toState
 * @extends{goog.events.Event}
 */
ble.tpaint.StateEvent = function(type, who, fromState, toState) {
  goog.events.Event.call(this, type);
  this.who = who;
  this.fromState = fromState;
  this.toState = toState;
}
goog.inherits(ble.tpaint.StateEvent, goog.events.Event);
