
roomID = ""
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
  data = {chat: jQuery("#chatText").val()}
  url = "/chat_" + roomID
  submit = (onError) ->
    postJSONRoundtrip(url, data, onReturn, onError, 10000)
  onError = () -> submit(onError)
  submit()
  return false)

submitPicture = () -> (
  onReturn = (jso) -> 0
  data = jQuery("#primaryCanvas")[0].toDataURL()
  url = "/savePicture"
  alert(data)
  submit = (onError) ->
    jQuery.ajax(
      url: url,
      data: data,
      type: "POST",
      dataType: "text"
      success: onReturn,
      error: onError,
      timeout: 10000)
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

jQuery(document).ready( () ->
  canvasFunctions.attachTo jQuery("#primaryCanvas")
  roomID = window.location.pathname.split("_")[1] 
  setRoomName = (name) ->
    jQuery("#roomName").text(name)
  jQuery.getJSON("/name_" + roomID, "", (data, _, __) -> setRoomName(data))
  jQuery("#chatForm").submit(submitChat) 
  jQuery("#savePictureForm").submit(submitPicture)
  setTimeout(eventPump, 10)
) 
