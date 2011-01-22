(function() {
  var alertStringify, eventPump, getJSON, myIndex, postJSON, postURL, roomID, roundtripJSON;
  roomID = "";
  myIndex = void 0;
  postURL = function() {
    return "message_" + roomID;
  };
  alertStringify = function(jso) {
    return alert(JSON.stringify(jso));
  };
  roundtripJSON = function(url, method, data, success, error, timeout) {
    var options;
    options = {
      method: method,
      postBody: JSON.stringify(data),
      onSuccess: function(transport) {
        var json;
        json = transport.responseText.evalJSON(true);
        return success(json);
      },
      onFailure: error
    };
    if (timeout !== void 0) {
      options.timeoutMilliseconds = timeout;
    }
    return new Ajax.Request(url, options);
  };
  postJSON = function(url, data, success, error, timeout) {
    return roundtripJSON(url, "POST", data, success, error, timeout);
  };
  getJSON = function(url, data, success, error, timeout) {
    return roundtripJSON(url, "GET", data, success, error, timeout);
  };
  eventPump = function(b) {
    var data, module, onReturn, url;
    data = "";
    url = "/messages_" + roomID;
    onReturn = void 0;
    module = {
      start: function() {
        return getJSON(url, data, onReturn, onReturn, 10000);
      }
    };
    onReturn = function(jsarray) {
      var imgUrl, jso, _i, _len;
      for (_i = 0, _len = jsarray.length; _i < _len; _i++) {
        jso = jsarray[_i];
        console.log(JSON.stringify(jso));
        if (jso.method === "chat") {
          b.chat.chat(jso.fromIndex - 1, jso.message);
        } else if (jso.method === "nameWasSet") {
          b.chat.setUserAtIndex(jso.userIndex - 1, jso.name);
        } else if (jso.method === "stroke") {
          b.canvas.drawStroke(jso);
        } else if (jso.method === "beginGame") {
          b.canvas.clear();
          b.canvas.setDoodleColor("#000000");
        } else if (jso.method === "stackReady") {
          b.canvas.clear();
          b.canvas.enable();
          b.passStackButton.enable();
          if (jso.imgUrl === "undefined") {
            imgUrl = void 0;
          } else {
            imgUrl = jso.imgUrl;
          }
          b.waitingStack.clearStacks();
          b.waitingStack.addToStacks(imgUrl);
        } else {
          console.log("unknown message");
        }
      }
      return setTimeout(module.start, 1);
    };
    return module;
  };
  document.observe("dom:loaded", function() {
    var binding, canvas, getMe, nameLayer, setRoomState;
    console.log("ready");
    roomID = window.location.pathname.split("_")[1];
    getMe = function(state) {
      var index, me, users, _ref;
      me = void 0;
      users = state.users;
      for (index = 0, _ref = users.length - 1; (0 <= _ref ? index <= _ref : index >= _ref); (0 <= _ref ? index += 1 : index -= 1)) {
        if (users[index].whoIs === "you") {
          me = users[index];
          myIndex = index;
        }
      }
      return me;
    };
    nameLayer = bindLayer($("namePrompt"));
    setRoomState = function(b) {
      return function(state) {
        var index, me, stroke, users, _i, _len, _ref, _ref2;
        $("roomName").update(state.name);
        me = getMe(state);
        console.log(state);
        if (me.name === void 0) {
          nameLayer.show();
        }
        if (me.name !== void 0) {
          nameLayer.hide();
        }
        if (me.isCreator && !state.inGame) {
          b.startGameButton.enable();
        }
        if (!state.inGame) {
          b.canvas.enable();
        }
        if (state.preGame) {
          _ref = state.preGame;
          for (_i = 0, _len = _ref.length; _i < _len; _i++) {
            stroke = _ref[_i];
            b.canvas.drawStroke(stroke);
          }
        }
        users = state.users;
        for (index = 0, _ref2 = users.length - 1; (0 <= _ref2 ? index <= _ref2 : index >= _ref2); (0 <= _ref2 ? index += 1 : index -= 1)) {
          b.chat.setUserAtIndex(index, users[index].name);
        }
        if (!state.inGame) {
          b.canvas.setDoodleColor(b.chat.colorFor(myIndex));
        }
        if (state.inGame) {
          b.canvas.setDoodleColor("#000000");
        }
        if (state.topStack) {
          b.passStackButton.enable();
          return b.canvas.enable();
        }
      };
    };
    $("nameForm").observe("submit", function(event) {
      var name, onError, onSuccess;
      name = $("nameText").getValue();
      postJSON(postURL(), {
        method: "name",
        name: name
      }, onSuccess = nameLayer.hide, onError = alertStringify);
      return Event.stop(event);
    });
    $("chatForm").observe("submit", function(event) {
      var message, onSuccess;
      message = $("chatText").getValue();
      onSuccess = function() {
        return $("chatText").value = "";
      };
      postJSON(postURL(), {
        method: "chat",
        chat: message
      }, onSuccess, alertStringify, 10000);
      return Event.stop(event);
    });
    canvas = canvasHandler($("primaryCanvas"), function(obj) {
      var error, rpc, success;
      success = function(result) {
        return void 0;
      };
      error = success;
      rpc = {
        method: "stroke",
        coordinates: obj.coordinates,
        color: obj.color
      };
      return postJSON(postURL(), rpc, success, error, 10000);
    });
    binding = {
      chat: chatManager($("playerNames"), $("chatSpace")),
      startGameButton: $("startGameButton"),
      passStackButton: $("passStackButton"),
      canvas: canvas,
      waitingStack: stacksManager($("waitingPile"))
    };
    $("startGameForm").observe("submit", function(event) {
      var onSuccess, rpc;
      rpc = {
        method: "startGame"
      };
      onSuccess = function() {
        $("startGameButton").disable();
        binding.canvas.clear();
        return binding.canvas.disable();
      };
      postJSON(postURL(), rpc, onSuccess, alertStringify, 10000);
      return Event.stop(event);
    });
    $("passStackForm").observe("submit", function(event) {
      var onError, onSuccess, rpc;
      rpc = {
        method: "passStack",
        newPicture: binding.canvas.getImage()
      };
      onSuccess = function() {
        binding.passStackButton.disable();
        binding.canvas.clear();
        return binding.waitingS;
      };
      onError = alertStringify;
      postJSON(postURL(), rpc, onSuccess, onError, 10000);
      return Event.stop(event);
    });
    getJSON("/state_" + roomID, "", setRoomState(binding), alertStringify);
    return eventPump(binding).start();
  });
}).call(this);
