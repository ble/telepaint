roomID = ""
myIndex = undefined
postURL = () -> "message_" + roomID
alertStringify = (jso) -> alert(JSON.stringify(jso))

roundtripJSON = (url, method, data, success, error, timeout) ->
  #console.log("json sent: #{JSON.stringify(data)}")
  options =
    method: method
    postBody: JSON.stringify(data)
    onSuccess: (transport) ->
      json = transport.responseText.evalJSON(true)
      #console.log("json received: #{transport.responseText}")
      success(json)
    onFailure: error
  options.timeoutMilliseconds = timeout if timeout isnt undefined
  new Ajax.Request(url, options)

postJSON = (url, data, success, error, timeout) ->
  roundtripJSON(url, "POST", data, success, error, timeout)

getJSON = (url, data, success, error, timeout) ->
  roundtripJSON(url, "GET", data, success, error, timeout)

eventPump = (b) ->
  data = ""
  url = "/messages_" + roomID
  onReturn = undefined
  module =
    start: () ->
      getJSON(url, data, onReturn, onReturn, 10000)
  onReturn = (jsarray) ->
    for jso in jsarray
      console.log(JSON.stringify(jso))
      if jso.method == "chat"
        b.chat.chat(jso.fromIndex-1, jso.message)
        #console.log("chat: #{jso.from},#{jso.whoIs}-> #{jso.message}")

      else if jso.method == "nameWasSet"
        b.chat.setUserAtIndex(jso.userIndex-1, jso.name)
        #console.log("name set: #{jso.userIndex},#{jso.whois} -> #{jso.name}")

      else if jso.method == "stroke"
        b.canvas.drawStroke(jso)

      else if jso.method == "beginGame"
        b.canvas.clear()
        #bugfix #0000
        #b.canvas.disable()
        b.canvas.setDoodleColor("#000000")

      else if jso.method == "stackReady"
        b.canvas.clear()
        b.canvas.enable()
        b.passStackButton.enable()
        if jso.imgUrl == "undefined"
          imgUrl = undefined
        else
          imgUrl = jso.imgUrl
        b.waitingStack.clearStacks()
        b.waitingStack.addToStacks(topImage: imgUrl)
      else if jso.method == "completedStack"
        urls = jso.urls
        urls.shift() while urls[0] == "none"
        urls.reverse()
        stack =
          sheets: ({url: url} for url in urls)
          topImage: urls[0]
        b.review.add(stack)
      else if jso.method == "gameDone"
        b.reviewLayer.show()
      else
        console.log("unknown message")

    setTimeout(module.start, 1)
  module









document.observe("dom:loaded", () -> (
  console.log("ready")
  roomID = window.location.pathname.split("_")[1]

  getMe = (state) ->
    me = undefined
    users = state.users
    for index in [0..users.length-1]
      if users[index].whoIs == "you"
        me = users[index]
        myIndex = index
    me

  nameLayer = bindLayer($("namePrompt"))

  setRoomState = (b) -> (state) ->
    $("roomName").update(state.name)
    me = getMe(state)
    console.log(state)
    nameLayer.show() if me.name is undefined
    nameLayer.hide() if me.name isnt undefined
    b.startGameButton.enable() if me.isCreator and not state.inGame
    b.canvas.enable() if not state.inGame
    if state.preGame
      b.canvas.drawStroke(stroke) for stroke in state.preGame

    users = state.users
    for index in [0..users.length-1]
      b.chat.setUserAtIndex(index, users[index].name)
    b.canvas.setDoodleColor(b.chat.colorFor(myIndex)) if not state.inGame
    b.canvas.setDoodleColor("#000000") if state.inGame
    if state.topStack
      b.passStackButton.enable()
      b.canvas.enable()

  $("nameForm").observe("submit", (event) ->
    name = $("nameText").getValue()
    postJSON(postURL(), {method: "name", name: name}, onSuccess = nameLayer.hide, onError = alertStringify)
    Event.stop(event)
  )

  $("chatForm").observe("submit", (event) ->
    message = $("chatText").getValue()
    onSuccess = () -> $("chatText").value = ""
    postJSON(postURL(), {method: "chat", chat: message}, onSuccess, alertStringify, 10000)
    Event.stop(event)
  )


  canvas = canvasHandler($("primaryCanvas"), (obj) ->
    success = (result) -> undefined
    error = success
    rpc = {method: "stroke", coordinates: obj.coordinates, color: obj.color}
    postJSON(postURL(), rpc, success, error, 10000))


  binding =
    chat: chatManager($("playerNames"), $("chatSpace"))
    startGameButton: $("startGameButton")
    passStackButton: $("passStackButton")
    canvas: canvas
    waitingStack: stacksManager($("waitingPile"), true)
    reviewLayer: bindLayer($("review"))
    review: reviewPileManager($("reviewPiles"), $("reviewStack"))

  $("startGameForm").observe("submit", (event) ->
    rpc = {method: "startGame"}
    onSuccess = () ->
      $("startGameButton").disable()
      binding.canvas.clear()
      binding.canvas.disable()
    postJSON(postURL(), rpc, onSuccess, alertStringify, 10000)
    Event.stop(event)
    )

  $("passStackForm").observe("submit", (event) ->
    rpc =
      method: "passStack"
      newPicture: binding.canvas.getImage()
    onSuccess = () ->
      binding.passStackButton.disable()
      binding.canvas.clear()
      binding.waitingS
    onError = alertStringify
    postJSON(postURL(), rpc, onSuccess, onError, 10000)
    Event.stop(event))



  getJSON("/state_" + roomID, "", setRoomState(binding), alertStringify)
  eventPump(binding).start()

  ))
