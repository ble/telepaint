(function() {
  var bindLayer, chatManager, reviewPileManager, rotater, stackManager, stacksManager, toggler;
  toggler = function(turnOn, turnOff) {
    var state;
    state = "off";
    return function() {
      if (state === "on") {
        turnOff();
        return state = "off";
      } else if (state === "off") {
        turnOn();
        return state = "on";
      }
    };
  };
  chatManager = function(playerContainer, chatContainer) {
    var colorMax, colorStep, delta, module, players, showingStacks, updateDisplay;
    players = [];
    colorMax = 11;
    colorStep = 3;
    delta = 2 * Math.PI * colorStep / colorMax;
    showingStacks = false;
    updateDisplay = function() {
      var x, _i, _j, _len, _len2, _results;
      if (showingStacks) {
        for (_i = 0, _len = players.length; _i < _len; _i++) {
          x = players[_i];
          x.element.update("" + x.name + ": " + x.count);
        }
      }
      if (!showingStacks) {
        _results = [];
        for (_j = 0, _len2 = players.length; _j < _len2; _j++) {
          x = players[_j];
          _results.push(x.element.update("" + x.name));
        }
        return _results;
      }
    };
    return module = {
      addPlayer: function(name) {
        var color, element;
        color = "#" + colorWheel(delta * players.length);
        element = new Element("li", {
          style: "color: " + color + ";"
        });
        playerContainer.insert(element);
        players.push({
          element: element,
          name: name,
          color: color,
          count: 0
        });
        return updateDisplay();
      },
      countPlayers: function() {
        return players.length;
      },
      toggleStacks: function() {
        showingStacks = !showingStacks;
        return updateDisplay();
      },
      chat: function(n, text) {
        var element, player;
        player = players[n];
        element = new Element("li", {
          style: "color: " + player.color + ";"
        });
        element.update(text);
        chatContainer.insert(element);
        return chatContainer.scrollTop = chatContainer.scrollHeight;
      }
    };
  };
  stacksManager = function(container) {
    var module, stacks;
    stacks = [];
    return module = {
      addToStacks: function(topImage) {
        var classes, inner, top;
        inner = new Element("div", {
          "class": "miniContainer"
        });
        inner.insert(new Element("div", {
          "class": "mini mini4"
        }));
        inner.insert(new Element("div", {
          "class": "mini mini3"
        }));
        inner.insert(new Element("div", {
          "class": "mini mini2"
        }));
        if (stacks.length === 0) {
          classes = "stackTop mini mini1";
        } else {
          classes = "mini mini1";
        }
        top = new Element("img", {
          "class": classes,
          src: topImage
        });
        inner.insert(top);
        container.insert(inner);
        return stacks.push([inner, top]);
      },
      clearStacks: function() {
        var stack, _i, _len;
        for (_i = 0, _len = stacks.length; _i < _len; _i++) {
          stack = stacks[_i];
          stack[0].remove();
        }
        return stacks = [];
      },
      removeStack: function() {
        var ignored, inner, _ref;
        _ref = stacks.shift(), inner = _ref[0], ignored = _ref[1];
        inner.remove();
        if (stacks.length > 0) {
          return stacks[0][1].addClassName("stackTop");
        }
      }
    };
  };
  stackManager = function(container) {
    var image, images, index, model, module, updateDisplay, _i, _len;
    images = (function() {
      var _i, _len, _ref, _results;
      _ref = [0, 1];
      _results = [];
      for (_i = 0, _len = _ref.length; _i < _len; _i++) {
        index = _ref[_i];
        _results.push(new Element("img", {
          width: 480,
          height: 360
        }));
      }
      return _results;
    })();
    for (_i = 0, _len = images.length; _i < _len; _i++) {
      image = images[_i];
      container.insert(image);
    }
    updateDisplay = function(urls) {
      var index, url, _ref, _results;
      _results = [];
      for (index = 0, _ref = images.length - 1; (0 <= _ref ? index <= _ref : index >= _ref); (0 <= _ref ? index += 1 : index -= 1)) {
        if (index < urls.length) {
          url = urls[index];
        }
        if (index >= urls.length) {
          url = "";
        }
        _results.push(images[index].writeAttribute("src", url));
      }
      return _results;
    };
    model = rotater([], updateDisplay);
    return module = {
      setStack: function(stack) {
        var sheet, urls;
        urls = (function() {
          var _i, _len, _ref, _results;
          _ref = stack.sheets;
          _results = [];
          for (_i = 0, _len = _ref.length; _i < _len; _i++) {
            sheet = _ref[_i];
            _results.push(sheet.url);
          }
          return _results;
        })();
        return model.replace(urls);
      },
      rotateStack: function() {
        return model.rotate();
      }
    };
  };
  rotater = function(start, onUpdate) {
    var module, state;
    state = start.slice();
    return module = {
      rotate: function() {
        state.push(state.shift());
        return onUpdate(state);
      },
      add: function(item) {
        state.push(item);
        return onUpdate(state);
      },
      replace: function(newState) {
        state = newState.slice();
        return onUpdate(state);
      }
    };
  };
  reviewPileManager = function(container, stackContainer) {
    var module, reviewStack, stacks, updateDisplay;
    stacks = stacksManager(container);
    reviewStack = stackManager(stackContainer);
    updateDisplay = function(state) {
      var stack, topStack, _i, _len, _results;
      if (state.length > 0) {
        topStack = state[0];
      }
      if (state.length === 0) {
        topStack = [];
      }
      reviewStack.setStack(topStack);
      stacks.clearStacks();
      _results = [];
      for (_i = 0, _len = state.length; _i < _len; _i++) {
        stack = state[_i];
        _results.push(stacks.addToStacks(stack.topImage));
      }
      return _results;
    };
    module = rotater([], updateDisplay);
    return module;
  };
  bindLayer = function(layer) {
    var result;
    layer = Element.extend(layer);
    return result = {
      show: function() {
        return layer.setStyle({
          visibility: "visible"
        });
      },
      hide: function() {
        return layer.setStyle({
          visibility: "hidden"
        });
      }
    };
  };
  window.toggler = toggler;
  window.chatManager = chatManager;
  window.stackManager = stackManager;
  window.rotater = rotater;
  window.reviewPileManager = reviewPileManager;
  window.bindLayer = bindLayer;
  window.stacksManager = stacksManager;
}).call(this);
