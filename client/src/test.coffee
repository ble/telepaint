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

document.observe("dom:loaded", () -> (
  console.log("ready")
  bound = bindLayer($("namePrompt"))
  $("showNamePrompt").observe("click", toggler(bound.show, bound.hide))
  bound = bindLayer($("review"))
  $("showReview").observe("click", toggler(bound.show, bound.hide)))
  stacksBound = stacksManager($("waitingPile"))
  count = 0
  $("addStack").observe("click", () ->
    stacksBound.addToStacks("test#{count}.png"
    count = count + 1))
  $("removeStack").observe("click", stacksBound.removeStack)
  boundCanvas = canvasHandler($("primaryCanvas"), (thing) -> console.log(JSON.stringify(thing)))
  $("activateCanvas").observe("click", boundCanvas.enable)
  $("deactivateCanvas").observe("click", boundCanvas.disable)

  chat = chatManager($("playerNames"), $("chatSpace"))
  $("addPlayer").observe("click", () -> chat.addPlayer("biftifffff"))
  $("chatButton").observe("click", (event) ->
    n = Math.floor(Math.random() * chat.countPlayers())
    chat.chat(n, $("chatText").getValue())
    $("chatText").value = ""
    Event.stop(event))
  review = reviewPileManager($("reviewPiles"), $("reviewStack"))
  ccount = 0
  $("addReviewStack").observe("click", () ->
    sheets = ({url: "foo#{ccount}#{index}.png"} for index in [0..5])
    topImage = "foo#{ccount}0.png"
    ccount = ccount + 1
    review.add(
      sheets: sheets
      topImage: topImage)
    )
  $("rotateReviewStacks").observe("click", review.rotate)

)
