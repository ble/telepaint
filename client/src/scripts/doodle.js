(function() {
  var ButtonEnabler, alertStringify, canvasHandler, chatHandler, displayChat, eventPump, getJSONRoundtrip, myName, nameHandler, passStackHandler, pictureHandler, postJSONRoundtrip, postURL, roomID, startGameHandler;
  roomID = "";
  myName = void 0;
  postURL = function() {
    return "message_" + roomID;
  };
  alertStringify = function(jso) {
    return alert(JSON.stringify(jso));
  };
  postJSONRoundtrip = function(url, data, success, error, timeout) {
    var settings;
    settings = {
      url: url,
      data: JSON.stringify(data),
      success: success,
      error: error,
      timeout: timeout,
      dataType: "json",
      type: "POST"
    };
    return jQuery.ajax(settings);
  };
  getJSONRoundtrip = function(url, data, success, error, timeout) {
    var settings;
    settings = {
      url: url,
      data: JSON.stringify(data),
      success: success,
      error: error,
      timeout: timeout,
      dataType: "json",
      type: "GET"
    };
    return jQuery.ajax(settings);
  };
  eventPump = {
    attachTo: function(canvas, gameStartHandler, passStackHandler) {
      var data, module, onReturn, url;
      data = "";
      url = "/messages_" + roomID;
      onReturn = void 0;
      module = {
        start: function() {
          return getJSONRoundtrip(url, data, onReturn, onReturn, 10000);
        }
      };
      onReturn = function(jso) {
        var obj, x;
        x = 0;
        while (x < jso.length) {
          obj = jso[x];
          if (obj.method === "chat") {
            displayChat("<li>" + obj.from + ": " + obj.message + "</li>", "");
          } else if (obj.method === "nameWasSet") {
            displayChat("<li>" + obj.name + " joined the par-tayyyy</li>", "userEntered");
          } else if (obj.method === "stroke") {
            canvas.drawStroke(obj);
          } else if (obj.method === "beginGame") {
            canvas.clear();
            passStackHandler.enable();
          } else if (obj.method === "stackReady") {
            displayChat("<li><img src=\"" + obj.imgUrl + "\"/></li>", "none");
          } else {
            alert(JSON.stringify(obj));
          }
          x++;
        }
        return setTimeout(module.start, 500);
      };
      return module;
    }
  };
  displayChat = function(element, style) {
    var selector;
    selector = jQuery("#chatSpace");
    selector.append(element);
    selector.children().last().addClass(style);
    return selector.attr("scrollTop", selector.attr("scrollHeight"));
  };
  nameHandler = {
    attachTo: function(prompt, form, text) {
      var onError, onSuccess, result, submitName, url;
      result = {
        hidePrompt: function() {
          return prompt.css("visibility", "hidden");
        },
        showPrompt: function() {
          return prompt.css("visibility", "visible");
        }
      };
      onSuccess = result.hidePrompt;
      onError = alertStringify;
      url = postURL();
      submitName = function() {
        var name, rpc;
        name = text.val();
        if (name === "") {
          return false;
        }
        rpc = {
          method: "name",
          name: name
        };
        postJSONRoundtrip(url, rpc, onSuccess, onError, 10000);
        return false;
      };
      form.submit(submitName);
      return result;
    }
  };
  ButtonEnabler = (function() {
    function ButtonEnabler(button) {
      this.button = button;
    }
    ButtonEnabler.prototype.enable = function() {
      return this.button.attr("disabled", "");
    };
    ButtonEnabler.prototype.disable = function() {
      return this.button.attr("disabled", "disabled");
    };
    return ButtonEnabler;
  })();
  chatHandler = {
    attachTo: function(form, text) {
      var onError, onSuccess, submitChat, url;
      onSuccess = function(jso) {
        return text.val("");
      };
      onError = alertStringify;
      url = postURL();
      submitChat = function() {
        var message, rpc;
        message = text.val();
        if (message === "") {
          return false;
        }
        rpc = {
          method: "chat",
          chat: message
        };
        postJSONRoundtrip(url, rpc, onSuccess, onError, 10000);
        return false;
      };
      return form.submit(submitChat);
    }
  };
  pictureHandler = {
    attachTo: function(form, canvas) {
      var onError, onSuccess, submitPicture, url;
      onSuccess = function(jso) {
        return void 0;
      };
      onError = alertStringify;
      url = postURL();
      submitPicture = function() {
        var picData, rpc;
        picData = canvas[0].toDataURL();
        rpc = {
          method: "passStack",
          newPicture: picData
        };
        postJSONRoundtrip(url, rpc, onSuccess, onError, 10000);
        return false;
      };
      return form.submit(submitPicture);
    }
  };
  startGameHandler = {
    attachTo: function(form, button, passStack) {
      var module, onError, onSuccess, rpc, submitStart, url;
      module = new ButtonEnabler(button);
      onSuccess = function(jso) {
        alert(JSON.stringify(jso));
        if (jso.status === "ok") {
          return module.disable();
        }
      };
      onError = alertStringify;
      url = postURL();
      rpc = {
        method: "startGame"
      };
      submitStart = function() {
        postJSONRoundtrip(url, rpc, onSuccess, onError, 10000);
        passStack.enable();
        return false;
      };
      form.submit(submitStart);
      return module;
    }
  };
  passStackHandler = {
    attachTo: function(form, button, canvas) {
      var module, onError, onSuccess, submitPass, url;
      module = new ButtonEnabler(button);
      onSuccess = function(jso) {
        canvas.clear();
        if (jso.inGame === false) {
          module.disable();
        }
        return alert(JSON.stringify(jso));
      };
      onError = alertStringify;
      url = postURL();
      submitPass = function() {
        var rpc;
        rpc = {
          method: "passStack",
          newPicture: canvas.getImage()
        };
        postJSONRoundtrip(url, rpc, onSuccess, onError, 10000);
        return false;
      };
      form.submit(submitPass);
      return module;
    }
  };
  canvasHandler = {
    attachTo: function(canvas) {
      var addLoc, context, ctxOf, down, getLoc, handleDown, handleEnter, handleMove, handleUp, module, mouse, points, postStroke, up, url;
      ctxOf = function(sel) {
        return sel[0].getContext("2d");
      };
      getLoc = function(sel, event) {
        var offset;
        offset = sel.offset();
        return {
          x: event.pageX - offset.left,
          y: event.pageY - offset.top
        };
      };
      addLoc = function(coords, loc) {
        return coords.unshift(loc.x, loc.y);
      };
      url = postURL();
      up = false;
      down = true;
      context = null;
      mouse = up;
      points = [];
      module = {
        drawStroke: function(obj) {
          var ctxPrivate, xys;
          xys = obj.coordinates.slice();
          ctxPrivate = ctxOf(canvas);
          ctxPrivate.beginPath();
          ctxPrivate.moveTo(xys.shift(), xys.shift());
          while (xys.length > 1) {
            ctxPrivate.lineTo(xys.shift(), xys.shift());
          }
          return ctxPrivate.stroke();
        },
        clear: function() {
          var _ctx;
          _ctx = ctxOf(canvas);
          return _ctx.clearRect(0, 0, canvas.width(), canvas.height());
        },
        getImage: function() {
          return canvas[0].toDataURL();
        }
      };
      postStroke = function(coordinates) {
        var error, rpc, success;
        success = function(result) {
          return void 0;
        };
        error = function(xhr, textStatus) {
          return debug(textStatus);
        };
        rpc = {
          method: "stroke",
          coordinates: coordinates
        };
        return postJSONRoundtrip(url, rpc, success, error, 10000);
      };
      handleDown = function(event) {
        var loc;
        mouse = down;
        loc = getLoc(canvas, event);
        addLoc(points, loc);
        context = ctxOf(canvas);
        context.beginPath();
        return context.moveTo(loc.x, loc.y);
      };
      handleEnter = function(event) {
        if (event.buttons !== 0) {
          return handleDown(event);
        }
      };
      handleUp = function(event) {
        handleMove(event);
        if (mouse === down) {
          mouse = up;
          postStroke(points.slice());
          return points = [];
        }
      };
      handleMove = function(event) {
        var loc;
        if (mouse === down) {
          loc = getLoc(canvas, event);
          addLoc(points, getLoc(canvas, event));
          context.lineTo(loc.x, loc.y);
          return context.stroke();
        }
      };
      canvas.bind('mousedown', handleDown);
      canvas.bind('mouseup', handleUp);
      canvas.bind('mousemove', handleMove);
      canvas.bind('mouseout', handleUp);
      return module;
    }
  };
  jQuery(document).ready(function() {
    var canvas, getMe, initializeState, name, passStack, pump, setRoomName, setRoomState, starter;
    roomID = window.location.pathname.split("_")[1];
    name = nameHandler.attachTo($("#prompt"), $("#nameForm"), $("#nameText"));
    chatHandler.attachTo($("#chatForm"), $("#chatText"));
    canvas = canvasHandler.attachTo($("#primaryCanvas"));
    pictureHandler.attachTo($("#savePictureForm"), $("#primaryCanvas"));
    passStack = passStackHandler.attachTo($("#passStackForm"), $("#passStackButton"), canvas);
    starter = startGameHandler.attachTo($("#startGameForm"), $("#startGameButton"), passStack);
    pump = eventPump.attachTo(canvas, starter, passStack);
    pump.start();
    setRoomName = function(name) {
      var roomName;
      roomName = jQuery("#roomName");
      return roomName.text(name);
    };
    getMe = function(state) {
      var me;
      me = state.users.filter(function(x) {
        return x.whoIs === "you";
      });
      if (me.length === 0) {
        return void 0;
      }
      return me[0];
    };
    setRoomState = function(state) {
      var me, stroke, _i, _len, _ref, _results;
      setRoomName(state.name);
      me = getMe(state);
      if (me.isCreator && !state.inGame) {
        starter.enable();
      }
      if (state.inGame) {
        passStack.enable();
      }
      if (me.name === void 0) {
        name.showPrompt();
      }
      if (me.name !== void 0) {
        name.hidePrompt();
      }
      _ref = state.preGame;
      _results = [];
      for (_i = 0, _len = _ref.length; _i < _len; _i++) {
        stroke = _ref[_i];
        _results.push(canvas.drawStroke(stroke));
      }
      return _results;
    };
    initializeState = function() {
      return jQuery.getJSON("/state_" + roomID, "", function(state, _, __) {
        return setRoomState(state);
      });
    };
    initializeState();
    return setTimeout(eventPump, 10);
  });
}).call(this);
