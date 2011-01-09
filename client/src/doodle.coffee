
roomID = ""
myName = undefined
PI = Math.PI

hexPad = (number, digits) ->
  result = number.toString(16)
  result = "0" + result while (result.length < digits)
  result

hexFormatColor = (red, green, blue) ->
  red = Math.floor(red)
  green = Math.floor(green)
  blue = Math.floor(blue)
  hexPad(red, 2) + hexPad(green, 2) + hexPad(blue, 2)

colorWheel = (radians) ->
  radians -= 2*PI while radians > 2*PI
  radians += 2*PI while radians < 0
  ixMajor = Math.floor(radians / (2*PI/3)) % 3
  ixMinor = (8 - Math.floor(radians / (PI/3))) % 3
  center = (1 + 2 * ixMajor) * PI/3
  amtMinor = 1.5 * Math.abs(radians - center)
  red = 0
  red = 255 if ixMajor == 0
  red = 255 * Math.sin(amtMinor) if ixMinor == 0
  green = 0
  green = 255 if ixMajor == 1
  green = 255 * Math.sin(amtMinor) if ixMinor == 1
  blue = 0
  blue = 255 if ixMajor == 2
  blue = 255 * Math.sin(amtMinor) if ixMinor == 2
  hexFormatColor(red, green, blue)

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



getLoc = (sel, event) ->
  offset = sel.offset()
  x: event.pageX - offset.left
  y: event.pageY - offset.top

addLoc = (coords, loc) ->
  coords.unshift loc.x, loc.y
ctxOf = (sel) ->
  sel[0].getContext("2d")

postStroke = (coordinates) ->
  msg = coordinates: coordinates
  data = JSON.stringify(msg)
  success = (result) -> undefined
  error = (xhr, textStatus) -> debug(textStatus)
  data = {method: "stroke", coordinates: coordinates}
  postJSONRoundtrip("message_"+roomID, data, success, error, 10000)

roomID = ""


drawStroke = (canvas, obj) ->
  cs = obj.coordinates.slice()
  context = ctxOf(canvas)
  context.beginPath()
  context.moveTo(cs.shift(), cs.shift())
  while cs.length >= 2
    context.lineTo(cs.shift(), cs.shift())
  context.stroke()

canvasFunctions =
  attachTo: (canvas) ->
    up = false
    down = true

    context = null
    mouse = up
    points = []
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

    canvas.bind('mousedown', handleDown)
    canvas.bind('mouseup', handleUp)
    canvas.bind('mousemove', handleMove)
    canvas.bind('mouseout', handleUp)
    drawLine: (xys) ->
      ctx.beginPath()
      ctx.moveTo xys.shift(), xys.shift()
      while xys.length > 1
        ctx.lineTo xys.shift(), xys.shift()
      ctx.stroke()

submitChat = () -> (
  onReturn = (jso) -> 0
  chatMessage = jQuery("#chatInput").val()
  return false if chatMessage == ""
  data = {method: "chat", chat: chatMessage}
  jQuery("#chatInput").val("")
  url = "/message_" + roomID
  submit = (onError) ->
    postJSONRoundtrip(url, data, onReturn, onError, 10000)
  onError = () -> submit(onError)
  submit()
  return false)

submitPicture = () -> (
  onReturn = (jso) -> alert(JSON.stringify(jso))
  picData = jQuery("#primaryCanvas")[0].toDataURL()
  data = {method: "picture", picture: picData}
  url = "/message_" + roomID
  submit = (onError) ->
    postJSONRoundtrip(url, data, onReturn, onError, 10000)
  onError = () -> submit(onError)
  submit()
  return false)


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



submitName = () -> (
  onReturn = (jso) ->
    if jso.name
      myName = jso.name
      jQuery("#prompt").css("visibility", "hidden")
    else if jso.status == "error"
      alert("Server error: " + jso.description)
  data = {method: "name", name: jQuery("#nameText").val()}
  url = "/message_" + roomID
  submit = (onError) ->
    postJSONRoundtrip(url, data, onReturn, onError, 10000)
  onError = () -> submit(onError)
  submit()
  return false)

jQuery(document).ready( () ->

  canvasFunctions.attachTo jQuery("#primaryCanvas")

  setRoomName = (name) ->
    roomName = jQuery("#roomName")
    roomName.text(name)
#    jQuery("#roomName").text(name)
#    angles = (x * 2 * PI / 40 for x in [0..80])
#    colors = (colorWheel(angle) for angle in angles)
#    doThing = (color) ->
#      fragment = "<div>#</div>"
#      res = roomName.append(fragment)
#      res.children().last().css("color", "#"+color).css("float", "left")
#    doThing(color) for color in colors


  getMyName = (state) ->
    me = state.users.filter( (x) -> x.whoIs == "you" )
    return undefined if me.length == 0
    return me[0].name


  setRoomState = (state) ->
    setRoomName(state.name)
    myName = getMyName(state)
    drawStroke(jQuery("#primaryCanvas"), stroke) for stroke in state.preGame
    jQuery("#prompt").css("visibility", "visible") if (myName == undefined)

  roomID = window.location.pathname.split("_")[1]
  jQuery.getJSON(
    "/state_" + roomID,
    "",
    (data, _, __) ->
      setRoomState(data)

    )

  jQuery("#chatForm").submit(submitChat)
  jQuery("#savePictureForm").submit(submitPicture)
  jQuery("#nameForm").submit(submitName)
  setTimeout(eventPump, 10)
)
