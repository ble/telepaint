
callInProgress = (xmlhttp) ->
  switch xmlhttp.readyState
    when 1 then true
    when 2 then true
    when 3 then true
    when 4 then false
    when 0 then false
    else false


timeoutFail = (request) ->
  console.log("timeoutFail called.")
  if callInProgress(request.transport)
    console.log("aborting request")
    request.options.onSuccess = () -> 0
    request.transport.abort()
    onFailure = request.options.onFailure
    onFailure(request.transport, request.json) if onFailure

timeoutClear = (request) ->
  console.log("timeoutClear called.")
  if request.options.timeoutMilliseconds
    window.clearTimeout(request['timeoutId'])
Ajax.Responders.register(
  onCreate: (request) ->
    if request.options.timeoutMilliseconds
      request['timeoutId'] = window.setTimeout( (() -> timeoutFail(request)), request.options.timeoutMilliseconds)
  onComplete: timeoutClear)

doThing = (url, timeout) ->
  options =
    method: "get",
    onSuccess: (transport) ->
      response = transport.responseText or "no response text"
      alert("Succeeded: #{response}")
    onFailure: () ->
      alert("Failed.")
  options.timeoutMilliseconds = timeout if timeout isnt undefined
  new Ajax.Request(url, options)

window.doThing = doThing
