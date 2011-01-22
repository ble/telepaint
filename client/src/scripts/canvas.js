(function() {
  var canvasHandler;
  canvasHandler = function(canvas, onStroke) {
    var addLoc, context, ctxOf, doodleColor, down, enabled, getLoc, handleDown, handleEnter, handleMove, handleUp, module, mouse, points, up;
    up = "up";
    down = "down";
    doodleColor = "#000000";
    ctxOf = function() {
      return canvas.getContext("2d");
    };
    getLoc = function(event) {
      var origin;
      origin = canvas.cumulativeOffset();
      return {
        x: event.pointerX() - origin.left,
        y: event.pointerY() - origin.top
      };
    };
    addLoc = function(coords, loc) {
      return coords.unshift(loc.x, loc.y);
    };
    enabled = false;
    up = false;
    down = true;
    context = null;
    mouse = up;
    points = [];
    module = {
      drawStroke: function(obj) {
        var color, ctxPrivate, xys;
        if (obj.color !== void 0) {
          color = obj.color;
        }
        if (obj.color === void 0) {
          color = "#000000";
        }
        xys = obj.coordinates.slice();
        ctxPrivate = ctxOf();
        ctxPrivate.strokeStyle = color;
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
        return _ctx.clearRect(0, 0, canvas.getWidth(), canvas.getHeight());
      },
      getImage: function() {
        return canvas.toDataURL();
      },
      setDoodleColor: function(color) {
        return doodleColor = color;
      },
      enable: function() {
        enabled = true;
        mouse = up;
        points = [];
        return context = null;
      },
      disable: function() {
        enabled = false;
        if (mouse === down) {
          onStroke({
            coordinates: points.slice(),
            color: doodleColor
          });
        }
        mouse = up;
        points = [];
        return context = null;
      }
    };
    handleDown = function(event) {
      var loc;
      if (enabled) {
        mouse = down;
        loc = getLoc(event);
        addLoc(points, loc);
        context = ctxOf(canvas);
        context.strokeStyle = doodleColor;
        context.beginPath();
        return context.moveTo(loc.x, loc.y);
      }
    };
    handleEnter = function(event) {
      if (enabled && event.buttons !== 0) {
        return handleDown(event);
      }
    };
    handleUp = function(event) {
      handleMove(event);
      if (enabled && mouse === down) {
        mouse = up;
        onStroke({
          coordinates: points.slice(),
          color: doodleColor
        });
        return points = [];
      }
    };
    handleMove = function(event) {
      var loc;
      if (enabled && mouse === down) {
        loc = getLoc(event);
        addLoc(points, loc);
        context.lineTo(loc.x, loc.y);
        return context.stroke();
      }
    };
    canvas.observe('mousedown', handleDown);
    canvas.observe('mouseup', handleUp);
    canvas.observe('mousemove', handleMove);
    canvas.observe('mouseout', handleUp);
    return module;
  };
  window.canvasHandler = canvasHandler;
}).call(this);
