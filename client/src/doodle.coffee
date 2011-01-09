
#global state
roomID = ""
myName = undefined

#PI = Math.PI
#
#hexPad = (number, digits) ->
#  result = number.toString(16)
#  result = "0" + result while (result.length < digits)
#  result
#
#hexFormatColor = (red, green, blue) ->
#  red = Math.floor(red)
#  green = Math.floor(green)
#  blue = Math.floor(blue)
#  hexPad(red, 2) + hexPad(green, 2) + hexPad(blue, 2)
#
#colorWheel = (radians) ->
#  radians -= 2*PI while radians > 2*PI
#  radians += 2*PI while radians < 0
#  ixMajor = Math.floor(radians / (2*PI/3)) % 3
#  ixMinor = (8 - Math.floor(radians / (PI/3))) % 3
#  center = (1 + 2 * ixMajor) * PI/3
#  amtMinor = 1.5 * Math.abs(radians - center)
#  red = 0
#  red = 255 if ixMajor == 0
#  red = 255 * Math.sin(amtMinor) if ixMinor == 0
#  green = 0
#  green = 255 if ixMajor == 1
#  green = 255 * Math.sin(amtMinor) if ixMinor == 1
#  blue = 0
#  blue = 255 if ixMajor == 2
#  blue = 255 * Math.sin(amtMinor) if ixMinor == 2
#  hexFormatColor(red, green, blue)
#
#    jQuery("#roomName").text(name)
#    angles = (x * 2 * PI / 40 for x in [0..80])
#    colors = (colorWheel(angle) for angle in angles)
#    doThing = (color) ->
#      fragment = "<div>#</div>"
#      res = roomName.append(fragment)
#      res.children().last().css("color", "#"+color).css("float", "left")
#    doThing(color) for color in colors


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

roomID = ""

eventPump = () -> (
  onReturn = (jso) ->
    x = 0
    while x < jso.length
      obj = jso[x]
      if obj.method == "chat"
        displayChat("<li>#{ obj.from }: #{ obj.message }</li>", "")
      else if obj.method == "nameWasSet"
        displayChat("<li>#{ obj.name } joined the par-tayyyy</li>", "userEntered")
      else if obj.method == "stroke"
        canvasSelector = jQuery("#primaryCanvas")
        drawStroke(canvasSelector, obj)
      else
        alert(JSON.stringify(obj))
      x++
    setTimeout(eventPump, 500)
  data = ""
  url = "/messages_" + roomID
  getJSONRoundtrip(url, data, onReturn, onReturn, 10000)
)

displayChat = (element, style) ->
  selector = jQuery("#chatSpace")
  selector.append(element)
  selector.children().last().addClass(style)
  selector.attr("scrollTop", selector.attr("scrollHeight"))

alertStringify = (jso) -> alert(JSON.stringify(jso))
postUrl = () -> "message_" + roomID


nameHandler =
  attachTo: (prompt, form, text) ->
    result =
      hidePrompt: () -> prompt.css("visibility", "hidden")
      showPrompt: () -> prompt.css("visibility", "visible")
    onSuccess = result.hidePrompt
    onError = alertStringify
    url = postUrl()
    submitName = () ->
      name = text.val()
      return false if name == ""
      rpc = {method: "name", name: name}
      postJSONRoundtrip(url, rpc, onSuccess, onError, 10000)
      return false
    form.submit(submitName)
    result



chatHandler =
  attachTo: (form, text) ->
    onSuccess = (jso) -> text.val("")
    onError = alertStringify
    url = postUrl()
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
    url = postUrl()
    submitPicture = () ->
      picData = canvas[0].toDataURL()
      rpc = {method: "passStack", newPicture: picData}
      postJSONRoundtrip(url, rpc, onSuccess, onError, 10000)
      return false
    form.submit(submitPicture)




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
    url = postUrl()

    #handler state
    up = false
    down = true
    context = null
    mouse = up
    points = []

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

    #external API
    drawStroke: (obj) ->
      xys = obj.coordinates.slice()
      ctxPrivate = ctxOf(canvas)
      ctxPrivate.beginPath()
      ctxPrivate.moveTo xys.shift(), xys.shift()
      while xys.length > 1
        ctxPrivate.lineTo xys.shift(), xys.shift()
      ctxPrivate.stroke()


jQuery(document).ready( () ->


  setRoomName = (name) ->
    roomName = jQuery("#roomName")
    roomName.text(name)

  roomID = window.location.pathname.split("_")[1]
  name = nameHandler.attachTo $("#prompt"), $("#nameForm"), $("#nameText")
  chatHandler.attachTo $("#chatForm"), $("#chatText")
  canvas = canvasHandler.attachTo $("#primaryCanvas")
  pictureHandler.attachTo $("#savePictureForm"), $("#primaryCanvas")
  

  getMyName = (state) ->
    me = state.users.filter( (x) -> x.whoIs == "you" )
    return undefined if me.length == 0
    return me[0].name

  setRoomState = (state) ->
    setRoomName(state.name)
    myName = getMyName(state)
    name.showPrompt() if myName == undefined
    name.hidePrompt() if myName /= undefined
    canvas.drawStroke(stroke) for stroke in state.preGame

  initializeState = () ->
    jQuery.getJSON("/state_" + roomID, "", (data, _, __) -> setRoomState(data))

  initializeState()
  setTimeout(eventPump, 10)

)
