
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
  success = (result) -> debug(result)
  error = (xhr, textStatus) -> debug(textStatus)
  postJSONRoundtrip("stroke", data, success, error, 10000)

roomID = ""

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
  data = {what: "chat", chat: jQuery("#chatText").val()}
  url = "/message_" + roomID
  submit = (onError) ->
    postJSONRoundtrip(url, data, onReturn, onError, 10000)
  onError = () -> submit(onError)
  submit()
  return false)

submitPicture = () -> (
  onReturn = (jso) -> alert(JSON.stringify(jso))
  picData = jQuery("#primaryCanvas")[0].toDataURL()
  data = {what: "picture", picture: picData}
  url = "/message_" + roomID
  submit = (onError) ->
    postJSONRoundtrip(url, data, onReturn, onError, 10000)
  onError = () -> submit(onError)
  submit()
  return false)


eventPump = () -> (
  onReturn = (jso) ->
    jQuery("#roomName").text(JSON.stringify(jso))#alert(jso)
    setTimeout(eventPump, 10)
  data = ""
  url = "/events"
  getJSONRoundtrip(url, data, onReturn, onReturn, 10000)
)

submitName = () -> (
  onReturn = (jso) ->
    if jso.name
      myName = jso.name
      jQuery("#prompt").css("visibility", "hidden")
    else if jso.status == "error"
      alert("Server error: " + jso.description)
  data = {what: "name", name: jQuery("#nameText").val()}
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
  #setTimeout(eventPump, 10)
)
