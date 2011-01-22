(function() {
  var alertStringify, getJSONRoundtrip, myName, postJSONRoundtrip, postURL, roomID;
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
  document.observe("dom:loaded", function() {
    var bound, boundCanvas, ccount, chat, count, review, stacksBound;
        console.log("ready");
    bound = bindLayer($("namePrompt"));
    $("showNamePrompt").observe("click", toggler(bound.show, bound.hide));
    bound = bindLayer($("review"));
    $("showReview").observe("click", toggler(bound.show, bound.hide));;
    stacksBound = stacksManager($("waitingPile"));
    count = 0;
    $("addStack").observe("click", function() {
      return stacksBound.addToStacks("test" + count + ".png", count = count + 1);
    });
    $("removeStack").observe("click", stacksBound.removeStack);
    boundCanvas = canvasHandler($("primaryCanvas"), function(thing) {
      return console.log(JSON.stringify(thing));
    });
    $("activateCanvas").observe("click", boundCanvas.enable);
    $("deactivateCanvas").observe("click", boundCanvas.disable);
    chat = chatManager($("playerNames"), $("chatSpace"));
    $("addPlayer").observe("click", function() {
      return chat.addPlayer("biftifffff");
    });
    $("chatButton").observe("click", function(event) {
      var n;
      n = Math.floor(Math.random() * chat.countPlayers());
      chat.chat(n, $("chatText").getValue());
      $("chatText").value = "";
      return Event.stop(event);
    });
    review = reviewPileManager($("reviewPiles"), $("reviewStack"));
    ccount = 0;
    $("addReviewStack").observe("click", function() {
      var index, sheets, topImage;
      sheets = (function() {
        var _results;
        _results = [];
        for (index = 0; index <= 5; index++) {
          _results.push({
            url: "foo" + ccount + index + ".png"
          });
        }
        return _results;
      })();
      topImage = "foo" + ccount + "0.png";
      ccount = ccount + 1;
      return review.add({
        sheets: sheets,
        topImage: topImage
      });
    });
    return $("rotateReviewStacks").observe("click", review.rotate);
  });
}).call(this);
