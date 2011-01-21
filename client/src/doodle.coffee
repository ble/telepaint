#global state
roomID = ""
myName = undefined
postURL = () -> "message_" + roomID
alertStringify = (jso) -> alert(JSON.stringify(jso))

postJSONRoundtrip = (url, data, success, error, timeout) ->
  settings =
    url: url
    data: JSON.stringify(data)
    success: success
    error: error
    timeout: timeout
    dataType: "json"
    type: "POST"
  jQuery.ajax(settings)

getJSONRoundtrip = (url, data, success, error, timeout) ->
  settings =
    url: url
    data: JSON.stringify(data)
    success: success
    error: error
    timeout: timeout
    dataType: "json"
    type: "GET"
  jQuery.ajax(settings)

#TODO: break the event pump and related functionality into its own module
eventPump =
  attachTo: (canvas, gameStartHandler, passStackHandler) ->
    data = ""
    url = "/messages_" + roomID
    onReturn = undefined
    module =
      start: () ->
        getJSONRoundtrip(url, data, onReturn, onReturn, 10000)
    onReturn = (jso) ->
      x = 0
      while x < jso.length
        obj = jso[x]
        if obj.method == "chat"
          displayChat("<li>#{ obj.from }: #{ obj.message }</li>", "")
        else if obj.method == "nameWasSet"
          displayChat("<li>#{ obj.name } joined the par-tayyyy</li>", "userEntered")
        else if obj.method == "stroke"
          canvas.drawStroke(obj)
        else if obj.method == "beginGame"
          canvas.clear()
          passStackHandler.enable()
        else if obj.method == "stackReady"
          displayChat("<li><img src=\"#{obj.imgUrl}\"/></li>", "none")
        else
          alert(JSON.stringify(obj))
        x++
      setTimeout(module.start, 500)
    #return
    module


displayChat = (element, style) ->
  selector = jQuery("#chatSpace")
  selector.append(element)
  selector.children().last().addClass(style)
  selector.attr("scrollTop", selector.attr("scrollHeight"))


nameHandler =
  attachTo: (prompt, form, text) ->
    result =
      hidePrompt: () -> prompt.css("visibility", "hidden")
      showPrompt: () -> prompt.css("visibility", "visible")
    onSuccess = result.hidePrompt
    onError = alertStringify
    url = postURL()
    submitName = () ->
      name = text.val()
      return false if name == ""
      rpc = {method: "name", name: name}
      postJSONRoundtrip(url, rpc, onSuccess, onError, 10000)
      return false
    form.submit(submitName)
    result

class ButtonEnabler
  constructor: (@button) ->
  enable: () ->
    @button.attr("disabled", "")
  disable: () ->
    @button.attr("disabled", "disabled")

chatHandler =
  attachTo: (form, text) ->
    onSuccess = (jso) -> text.val("")
    onError = alertStringify
    url = postURL()
    submitChat = () ->
      message = text.val()
      return false if message == ""
      rpc = {method: "chat", chat: message}
      postJSONRoundtrip(url, rpc, onSuccess, onError, 10000)
      return false
    form.submit(submitChat)

pictureHandler =
  attachTo: (form, canvas) ->
    onSuccess = (jso) -> undefined
    onError = alertStringify
    url = postURL()
    submitPicture = () ->
      picData = canvas[0].toDataURL()
      rpc = {method: "passStack", newPicture: picData}
      postJSONRoundtrip(url, rpc, onSuccess, onError, 10000)
      return false
    form.submit(submitPicture)

startGameHandler =
  attachTo: (form, button, passStack) ->
    module = new ButtonEnabler(button)

    onSuccess = (jso) ->
      alert(JSON.stringify(jso))
      module.disable() if jso.status == "ok"

    onError = alertStringify
    url = postURL()
    rpc = {method: "startGame"}
    submitStart = () ->
     postJSONRoundtrip(url, rpc, onSuccess, onError, 10000)
     passStack.enable()
     return false
    form.submit(submitStart)
    module

passStackHandler =
  attachTo: (form, button, canvas) ->
    module = new ButtonEnabler(button)
    onSuccess = (jso) ->
      canvas.clear()
      module.disable() if jso.inGame == false
      alert(JSON.stringify(jso))
    onError = alertStringify
    url = postURL()
    submitPass = () ->
      rpc = {method: "passStack", newPicture: canvas.getImage()}
      postJSONRoundtrip(url, rpc, onSuccess, onError, 10000)
      return false
    form.submit(submitPass)

    module


canvasHandler =

  attachTo: (canvas) ->
    #helpers
    ctxOf = (sel) ->
      sel[0].getContext("2d")

    getLoc = (sel, event) ->
      offset = sel.offset()
      x: event.pageX - offset.left
      y: event.pageY - offset.top

    addLoc = (coords, loc) ->
      coords.unshift loc.x, loc.y

    #delayed evaluation
    url = postURL()

    #handler state
    up = false
    down = true
    context = null
    mouse = up
    points = []

    #external API
    module =
      drawStroke: (obj) ->
        xys = obj.coordinates.slice()
        ctxPrivate = ctxOf(canvas)
        ctxPrivate.beginPath()
        ctxPrivate.moveTo xys.shift(), xys.shift()
        while xys.length > 1
          ctxPrivate.lineTo xys.shift(), xys.shift()
        ctxPrivate.stroke()
      clear: () ->
        _ctx = ctxOf(canvas)
        _ctx.clearRect(0, 0, canvas.width(), canvas.height())
      getImage: () ->
        canvas[0].toDataURL()
        
    #XHR
    postStroke = (coordinates) ->
      success = (result) -> undefined
      error = (xhr, textStatus) -> debug(textStatus)
      rpc = {method: "stroke", coordinates: coordinates}
      postJSONRoundtrip(url, rpc, success, error, 10000)

    #canvas interaction handlers
    handleDown = (event) ->
      mouse = down
      loc = getLoc(canvas, event)
      addLoc points, loc
      context = ctxOf(canvas)
      context.beginPath()
      context.moveTo(loc.x, loc.y)

    handleEnter = (event) ->
      if event.buttons isnt 0
        handleDown event

    handleUp = (event) ->
      handleMove(event)
      if mouse is down
        mouse = up
        postStroke(points.slice())
        points = []
        
    handleMove = (event) ->
      if mouse is down
        loc = getLoc(canvas, event)
        addLoc points, getLoc(canvas, event)
        context.lineTo(loc.x, loc.y)
        context.stroke()

    #binding
    canvas.bind('mousedown', handleDown)
    canvas.bind('mouseup', handleUp)
    canvas.bind('mousemove', handleMove)
    canvas.bind('mouseout', handleUp)
    #return
    module


jQuery(document).ready( () ->


  roomID = window.location.pathname.split("_")[1]
  name = nameHandler.attachTo $("#prompt"), $("#nameForm"), $("#nameText")
  chatHandler.attachTo $("#chatForm"), $("#chatText")
  canvas = canvasHandler.attachTo $("#primaryCanvas")
  pictureHandler.attachTo $("#savePictureForm"), $("#primaryCanvas")
  passStack = passStackHandler.attachTo $("#passStackForm"), $("#passStackButton"), canvas
  starter = startGameHandler.attachTo $("#startGameForm"), $("#startGameButton"), passStack
  pump = eventPump.attachTo(canvas, starter, passStack)
  pump.start()

  
  #TODO: break out state-setting into its own "handler" / module
  setRoomName = (name) ->
    roomName = jQuery("#roomName")
    roomName.text(name)

  getMe = (state) ->
    me = state.users.filter( (x) -> x.whoIs == "you" )
    return undefined if me.length == 0
    return me[0]

  setRoomState = (state) ->
    setRoomName(state.name)
    me = getMe(state)
    starter.enable() if me.isCreator and not state.inGame
    passStack.enable() if state.inGame
    name.showPrompt() if me.name is undefined
    name.hidePrompt() if me.name isnt undefined
    canvas.drawStroke(stroke) for stroke in state.preGame

  initializeState = () ->
    jQuery.getJSON("/state_" + roomID, "", (state, _, __) -> setRoomState(state))

  initializeState()
  setTimeout(eventPump, 10)

)
