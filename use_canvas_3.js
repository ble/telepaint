goog.provide('ble.use_canvas_3');

goog.require('ble.scribble.Simultaneous');
goog.require('ble.scribble.Canvas');
goog.require('ble.scribble.style.StylePicker');
goog.require('ble.scribble.style.EventType');

goog.require('goog.events');
goog.require('goog.ui.Menu');

goog.require('goog.math.Vec2');
goog.require('ble.traj');
//goog.require('ble._2d.MovingPip');
//goog.require('ble.traj');

ble.use_canvas_3 = function() {
  var drawingSize = new goog.math.Size(640, 480);
  var smallDrawingSize = new goog.math.Size(213, 160);
  var dom = new goog.dom.DomHelper();
  var container = dom.getElement("outermost");
  var scribble = new ble.scribble.UI(drawingSize.width, drawingSize.height);
  scribble.render(container);
  var withContext_reacharound = function(action) {
    scribble.canvas.withContext(action);
  };


  var scribbles = new ble.Scribbles();

  var get9Drawings = function() {
    var drawings = [];
    var data = scribbles.data;
    for(var key in data) {
      if(data.hasOwnProperty(key))
        drawings.push(data[key]);
      if(drawings.length == 9)
        break;
    }
    if(drawings.length == 0)
      return drawings;

    for(var i = 0; drawings.length < 9; i++)
      drawings.push(drawings[i]);
    return drawings.map(function(drawing) {
      return new ble.scribble.Drawing(0, drawing);
    });
  };

  var makeSeq = function() {
    var drawingSize = new goog.math.Size(640, 480);
    var start = new goog.math.Vec2(320, 360);
    var fromAngle = Math.PI / 2;
    var radius = 120;
    var toAngle = 3 * Math.PI / 2;
    var trajCenter = ble.traj.arc(start, fromAngle, radius, toAngle);

    var drawnSize = new goog.math.Size(320, 240);
    var traj = ble.traj.fixedSizeBoxTraj(trajCenter, drawnSize);

    return new ble.scribble.Sequence(get9Drawings(), drawingSize, traj, 3000);
  };

  var makeMulti = function() {
    return new ble.scribble.Simultaneous(
        0,
        get9Drawings(),
        3,
        drawingSize,
        smallDrawingSize); 
  };


  var animating = false;

  var aniseq = function() {
    if(animating)
      return;
    var seq = makeSeq().withLength(60000);
    var drawAt = function(time) {
      withContext_reacharound(function(ctx) {
        ctx.clearRect(0, 0, drawingSize.width, drawingSize.height);
        seq.at(time).draw(ctx);
      });
    };
    var seqRAF = function() {
      var delta = Date.now() - start;
      drawAt(delta);
      if(delta >= seq.length())
        animating = false;
      else
        window.webkitRequestAnimationFrame(seqRAF); 
    };
    var seqInterval = function() {
      var delta = Date.now() - start;
      drawAt(delta);
      if(delta >= seq.length()) {
        animating = false;
        window.clearInterval(intervalHandle);
      }
    };

    animating = true;
    var start = Date.now(); 

    if(window.webkitRequestAnimationFrame) {
      seqRAF();
    } else {
      var intervalHandle = window.setInterval(seqInterval, 15);
    }
  };

  var animulti = function() {
    if(animating)
      return;
    var multi = makeMulti().withLength(3000);
    if(window.webkitRequestAnimationFrame) {
      animateRAF(multi);
    } else {
      animateInterval(multi);
    }
  };

  var animateRAF = function(multi) {
    animating = true;
    var start = Date.now();
    var length = multi.length();
    var redraw = function(now) {
      var delta = now - start;
      withContext_reacharound(function(ctx) {
        ctx.clearRect(0, 0, drawingSize.width, drawingSize.height);
        multi.at(delta).draw(ctx);
      });
      
      if(delta >= length)
        finishAnimation();
      else
        window.webkitRequestAnimationFrame(redraw);
    };
    window.webkitRequestAnimationFrame(redraw);
  };

  var animateInterval = function(multi) {
    animating = true;
    var start = Date.now();
    var length = multi.length();
    var handle;
    var redraw = function() {
      var delta = Date.now() - start;
      withContext_reacharound(function(ctx) {
        ctx.clearRect(0, 0, drawingSize.width, drawingSize.height);
        multi.at(delta).draw(ctx);
      });
      if(delta >= length) {
        window.clearInterval(handle);
        finishAnimation();
      }
    };
    handle = window.setInterval(redraw, 15); 
  };

  var finishAnimation = function() {
    animating = false;
  };






  var createMenu = function() {
    var menu = new goog.ui.Menu();
    var makeNew = new goog.ui.MenuItem('New');
    var replay = new goog.ui.MenuItem('Replay');
    var save = new goog.ui.MenuItem('Save');
    var nineUp = new goog.ui.MenuItem('9 up');
    var nineUpReplay = new goog.ui.MenuItem('9 up replay');
    var sequentialReplay = new goog.ui.MenuItem('sequence replay');
    var json = new goog.ui.MenuItem('Display JSON');
    
    menu.addChild(save, true);
    menu.addChild(makeNew, true);
    menu.addChild(replay, true);
    menu.addChild(json, true);
    menu.addChild(nineUp, true);
    menu.addChild(nineUpReplay, true);
    menu.addChild(sequentialReplay, true);
    menu.addChild(new goog.ui.MenuSeparator(), true);
    menu.render(container);
    menu.getElement().style["position"] = "relative";
    menu.getElement().style["display"] = "inline-block";

    if(scribbles.keys.length > 0)
      scribbles.currentKey = scribbles.keys[0];
    var scribbleMenuItems = [];
    var updateMenu = function() {
      for(var i = 0; i < scribbleMenuItems.length; i++) {
        menu.removeChild(scribbleMenuItems[i], true);
      }
      scribbleMenuItems = [];
      for(var i = 0; i < scribbles.keys.length; i++) {
        var key = scribbles.keys[i];
        var label = new Date(key).toString();
        var item = new goog.ui.MenuItem(label);
        item._key_ = key;
        menu.addChild(item, true);
        scribbleMenuItems.push(item);
      }
    };
    updateMenu();


    var pickle = dom.createDom("pre");
    pickle.style["display"] = "none";
    pickle.style["width"] = "400px";
    pickle.style["height"] = "400px";
    pickle.style["overflow"] = "scroll";
    pickle.style["border"] = "1px solid black";
    dom.appendChild(container, pickle);
    goog.events.listen(scribbles, ble.Scribbles.EventType.UPDATE, updateMenu);

    var drawCurrent = function() {
      if(scribbles.currentKey != null) {
        var items = scribbles.data[scribbles.currentKey];
        var start = 0;
        if(items.length > 0)
          start = items[0].start();
        scribble.setPicture(start, items);
      }
    };
    goog.events.listen(menu, goog.ui.Component.EventType.ACTION, function(e) {

      var target = e.target;
      if(target === makeNew) {
        scribbles.makeNew();
        drawCurrent();
      } else if(target === replay) { 
        var replayLength = scribble.getPicture().length * 500;
        scribble.canvas.replayAll(replayLength);
      } else if(target === save) {
        scribbles.save(scribble.getPicture());         
      } else if(target == json) {
        dom.removeChildren(pickle);
        var link = dom.createElement("a");
        var data = scribbles.pickle();
        link.href="data:application/json;charset=utf-8," + data;
        dom.appendChild(link, dom.createTextNode("JSON"));
        dom.appendChild(pickle, link);
        dom.appendChild(pickle, dom.createTextNode(scribbles.pickle()));
        pickle.style["display"] = "block";
      } else if(target === nineUp) {
        withContext_reacharound(function(ctx) {
          ctx.clearRect(0, 0, drawingSize.width, drawingSize.height);
          makeMulti().draw(ctx);
        });
      } else if(target === nineUpReplay) {
        animulti();
      } else if(target === sequentialReplay) {
        aniseq();
      } else if(goog.isDef(target._key_)) {
        if(goog.isDef(scribbles.data[target._key_])) {
          scribbles.currentKey = target._key_;
          drawCurrent();
        }
      }
    });

  };
  createMenu();
};

goog.exportSymbol('ble.use_canvas_3', ble.use_canvas_3);
