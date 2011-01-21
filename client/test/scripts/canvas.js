(function() {
  var canvasHandler;
  canvasHandler = function(canvas, onStroke) {
    var addLoc, context, ctxOf, down, enabled, getLoc, handleDown, handleEnter, handleMove, handleUp, module, mouse, points, up;
    up = "up";
    down = "down";
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
        var ctxPrivate, xys;
        xys = obj.coordinates.slice();
        ctxPrivate = ctxOf();
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
        return canvas.toDataURL();
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
          onStroke(points.slice());
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
        onStroke(points.slice());
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
