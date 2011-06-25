
goog.provide('ble.tpaint.stateTransition');
goog.provide('ble.tpaint.actions');
goog.scope(function() {
  goog.require('ble.tpaint.EventTypes');
  goog.require('ble.tpaint.States');


  var States = ble.tpaint.States;
  var EventTypes = ble.tpaint.EventTypes;
  (function() {

    var addTransition = function(obj, state0, event, state1) {
      if(obj[state0] === undefined)
        obj[state0] = {};
      obj[state0][event] = state1;
    }

    var addAction = function(obj, state, event) {
      addTransition(obj, state, event, state);
    }

    var addActionMultipleState = function(obj, states, event) {
      for(var i = 0; i < states.length; i++) {
        addAction(obj, states[i], event);
      }
    };

    var chatStates = [
       States.PICKED_PSEUDONYM,
       States.PREGAME,
       States.DRAWING,
       States.WAITING,
       States.PEEKING,
       States.WALLHAXING,
       States.REVIEWING];

    var drawStates = [
       States.DRAWING,
       States.PREGAME];

    var receiveStates = [
       States.PREGAME,
       States.PEEKING,
       States.OBSERVING];

    var v = {};
    addTransition(v, null,                    EventTypes.ENTER_LOBBY,     States.IN_LOBBY);
    addTransition(v, States.IN_LOBBY,         EventTypes.PICK_PSEUDONYM,  States.PICKED_PSEUDONYM);
    addTransition(v, States.PICKED_PSEUDONYM, EventTypes.JOIN_PREGAME,    States.PREGAME);
    addTransition(v, States.PICKED_PSEUDONYM, EventTypes.OBSERVE,         States.OBSERVING);
    addTransition(v, States.PREGAME,          EventTypes.START_GAME,      States.DRAWING);
    addTransition(v, States.DRAWING,          EventTypes.PASS_DRAWING,    States.WAITING);
    addTransition(v, States.WAITING,          EventTypes.PEEK,            States.PEEKING);
    addTransition(v, States.WAITING,          EventTypes.WALLHAX,         States.WALLHAXING);
    
    addActionMultipleState(v, chatStates, EventTypes.CHAT);
    addActionMultipleState(v, drawStates, EventTypes.STROKE);

    var i = {};
    addTransition(i, States.IN_LOBBY,         EventTypes.START_GAME,      States.OBSERVING);
    addTransition(i, States.PICKED_PSEUDONYM, EventTypes.START_GAME,      States.OBSERVING);
    addTransition(i, States.PREGAME,          EventTypes.START_GAME,      States.DRAWING);
    addTransition(i, States.WAITING,          EventTypes.RECEIVE_DRAWING, States.DRAWING);
    addTransition(i, States.PEEKING,          EventTypes.RECEIVE_DRAWING, States.DRAWING);
    addTransition(i, States.WALLHAXING,       EventTypes.RECEIVE_DRAWING, States.DRAWING);
    addTransition(i, States.WAITING,          EventTypes.COMPLETE,        States.REVIEWING);

    addActionMultipleState(i, chatStates, EventTypes.CHAT);
    addActionMultipleState(i, receiveStates, EventTypes.STROKE);

    var stateTransition = function(isVoluntary, currentState, event) {
      var table = isVoluntary ? v : i;
      if(table[currentState] && table[currentState][event])
        return table[currentState][event];
      else
        return false;
    };
    ble.tpaint.stateTransition = stateTransition;

    var actions = function(currentState) {
      var result = [];
      var readFrom = v[currentState];
      if(readFrom) {
        for(var i in readFrom) {
          if(readFrom.hasOwnProperty(i))
            result.push(i);
        }
      }
      return result;
    };
    ble.tpaint.actions = actions;
  })();
});
  /* 
        var i = {
      States.IN_LOBBY:
      { EventTypes.START_GAME: States.OBSERVING },

      States.PICKED_PSEUDONYM:
      { EventTypes.START_GAME: States.OBSERVING,
        EventTypes.CHAT: States.PICKED_PSEUDONYM },

      States.PREGAME:
      { EventTypes.START_GAME: States.DRAWING,
        EventTypes.STROKE: States.PREGAME,
        EventTypes.CHAT: States.PREGAME },

      States.DRAWING:
      { EventTypes.CHAT: States.DRAWING },

      States.WAITING:
      { EventTypes.CHAT: States.WAITING,
        EventTypes.RECEIVE_DRAWING: States.DRAWING
        EventTypes.COMPLETE: States.REVIEWING },

      States.PEEKING:
      { EventTypes.CHAT: States.PEEKING,
        EventTypes.STROKE: States.PEEKING,
        EventTypes.RECEIVE_DRAWING: States.DRAWING },

      States.WALLHAXING:
      { EventTypes.CHAT: States.WALLHAXING,
        EventTypes.RECEIVE_DRAWING: States.DRAWING },
      }
    };
var v = {
      null:
      { EventTypes.ENTER_LOBBY: States.IN_LOBBY},

      States.IN_LOBBY:
      { EventTypes.PICK_PSEUDONYM: States.PICKED_PSEUDONYM},

      States.PICKED_PSEUDONYM:
      { EventTypes.OBSERVE: States.OBSERVING,
        EventTypes.JOIN_PREGAME: States.PREGAME,
        EventTypes.CHAT: States.PICKED_PSEUDONYM},

      States.PREGAME:
      { EventTypes.START_GAME: States.DRAWING,
        EventTypes.STROKE: States.PREGAME,
        EventTypes.CHAT: States.PREGAME},
      
      States.DRAWING:
      { EventTypes.PASS_DRAWING: States.WAITING,
        EventTypes.STROKE: States.DRAWING,
        EventTypes.CHAT: States.DRAWING},

      States.WAITING:
      { EventTypes.PEEK: States.PEEKING,
        EventTypes.WALLHAX: States.WALLHAXING,
        EventTypes.CHAT: States.WAITING},
      
      States.PEEKING:
      { EventTypes.WALLHAX: States.WALLHAXING,
        EventTypes.CHAT: States.PEEKING},

      States.WALLHAXING:
      { EventTypes.PEEK: States.PEEKING,
        EventTypes.CHAT: States.WALLHAXING}
    };


    ble.tpaint.transition.Voluntary = v;
    ble.tpaint.transition.Involuntary = i;
          events.push(transition.by);
        }
      }
      return events;
    };
*/

