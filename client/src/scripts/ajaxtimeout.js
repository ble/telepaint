(function() {
  var callInProgress, doThing, timeoutClear, timeoutFail;
  callInProgress = function(xmlhttp) {
    switch (xmlhttp.readyState) {
      case 1:
        return true;
      case 2:
        return true;
      case 3:
        return true;
      case 4:
        return false;
      case 0:
        return false;
      default:
        return false;
    }
  };
  timeoutFail = function(request) {
    var onFailure;
    console.log("timeoutFail called.");
    if (callInProgress(request.transport)) {
      console.log("aborting request");
      request.options.onSuccess = function() {
        return 0;
      };
      request.transport.abort();
      onFailure = request.options.onFailure;
      if (onFailure) {
        return onFailure(request.transport, request.json);
      }
    }
  };
  timeoutClear = function(request) {
    console.log("timeoutClear called.");
    if (request.options.timeoutMilliseconds) {
      return window.clearTimeout(request['timeoutId']);
    }
  };
  Ajax.Responders.register({
    onCreate: function(request) {
      if (request.options.timeoutMilliseconds) {
        return request['timeoutId'] = window.setTimeout((function() {
          return timeoutFail(request);
        }), request.options.timeoutMilliseconds);
      }
    },
    onComplete: timeoutClear
  });
  doThing = function(url, timeout) {
    var options;
    options = {
      method: "get",
      onSuccess: function(transport) {
        var response;
        response = transport.responseText || "no response text";
        return alert("Succeeded: " + response);
      },
      onFailure: function() {
        return alert("Failed.");
      }
    };
    if (timeout !== void 0) {
      options.timeoutMilliseconds = timeout;
    }
    return new Ajax.Request(url, options);
  };
  window.doThing = doThing;
}).call(this);
