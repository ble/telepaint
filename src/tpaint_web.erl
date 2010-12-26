%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for tpaint.

-module(tpaint_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
  {DocRoot, Options1} = get_option(docroot, Options),
  Loop = fun (Req) ->
    ?MODULE:loop(Req, DocRoot)
  end,
  {ok, _} = nexus:start_link(),
  mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
  mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
  "/" ++ Path = Req:get(path),
  io:format("~p:~p~n", [Req:get(path), Req:get(method)]),
  try
    case Req:get(method) of
      Method when Method =:= 'GET'; Method =:= 'HEAD' ->
        case Path of
          "room_" ++ ShortID ->
            handleShowRoom(Req, ShortID, DocRoot);
          "name_" ++ ShortID ->
            tpaint_util:pr("foo"),
            handleName(Req, ShortID);
          "events" ->
            handleEvents(Req);
          _ ->
            Req:serve_file(Path, DocRoot)
        end;
      'POST' ->
        case Path of
          "makeRoom" ->
            handleMakeRoom(Req);
          "chat_" ++ ShortID ->
            handleChat(Req);
          "savePicture" ->
            handleSavePicture(Req);
          _ ->
            Req:not_found()
        end;
      _ ->
        Req:respond({501, [], []})
    end
  catch
    Type:What ->
      Report = ["web request failed",
                {path, Path},
                {type, Type}, {what, What},
                {trace, erlang:get_stacktrace()}],
      error_logger:error_report(Report),
      %% NOTE: mustache templates need \ because they are not awesome.
      Req:respond({500, [{"Content-Type", "text/plain"}],
                   "request failed, sorry\n"})
  end.


handleShowRoom(Req, ShortID, DocRoot) ->
  RespondSetCookies = fun () ->
    case nexus:newUserIn(ShortID, Req:get(peer)) of 
      {ok, {{UserKey, UserID}, {RoomKey, UID}}} ->
        Cookies = roomCookies(RoomKey, UID, UserKey, UserID),
        Req:serve_file("doodle.html", DocRoot, Cookies);
      _ ->
        tpaint_util:textResponse(Req, "couldn't create another user in that room.")
    end
  end,
  case {Req:get_cookie_value("roomKey"), Req:get_cookie_value("roomID")} of
    {undefined, undefined} ->
      RespondSetCookies();
    {CStrKey, CookieRoomID} ->
      CookieKey = erlang:list_to_atom(CStrKey),
      case Vals=nexus:getRoomInfo(ShortID) of
        {ok, {CookieKey, _, CookieRoomID, _}} ->
          Req:serve_file("doodle.html", DocRoot);
        _ -> 
          tpaint_util:pr(Vals),
          RespondSetCookies()
      end
 end.

handleName(Req, ShortID) ->
  case nexus:getRoomInfo(ShortID) of
    {ok, {_, _, _, RoomName}} ->
      tpaint_util:respondJSON(Req, RoomName);
    _ ->
      tpaint_util:respondJSON(Req, "no such room")
  end.

handleChat(Req) ->
  Rk = Req:get_cookie_value("roomKey"),
  Uk = Req:get_cookie_value("userKey"),
  if
    Rk =:= undefined orelse Uk =:= undefined ->
      tpaint_util:textResponse(Req, "\"invalid\"");
    true ->
      JSON = Req:recv_body(),
      UserKey = list_to_atom(Uk),
      JSO = mochijson:decode(JSON),
      case userServer:sendMessage(UserKey, JSO) of
        ok -> tpaint_util:textResponse(Req, "ok")
      end 
  end.

handleSavePicture(Req) ->
  Uk = Req:get_cookie_value("userKey"),
  case Uk of
    undefined ->
      tpaint_util:textResponse(Req, "\"invalid\"");
    _ ->
      UserKey = list_to_atom(Uk),
      Data = binary_to_list(Req:recv_body()),
      case userServer:savePicture(UserKey, Req:get(peer), Data) of
        {ok, Path} -> tpaint_util:textResponse(Req, Path);
        _ -> tpaint_util:textResponse(Req, "failed")
      end
  end.

%handleShowRoom(Req, ShortID, DocRoot) ->
%  case Req:get_cookie_value("roomKey") of
%    undefined -> 
%      case nexus:getRoomInfo(ShortID) of
%        {ok, RoomKey, RoomLong} ->
%
%  Req:serve_file("doodle.html", DocRoot).
%  case Req:get_cookie_value("roomKey") of
%    undefined -> cookieAndShowRoom(Req, ShortID)

% showRoomNewVisitor(Req, 
roomCookies(RoomKey, RoomLong, UserKey, UserID) ->
  CookieSpec = [
      {"roomID", RoomLong},
      {"roomKey", RoomKey},
      {"userID", UserID},
      {"userKey", UserKey}],
  tpaint_util:cookies(CookieSpec).

handleMakeRoom(Req) ->
  Params = dict:from_list(Req:parse_post()),
  case dict:find("roomName", Params) of
    error ->
      tpaint_util:redirectTo(Req, "/createRoom.html", []);
    {ok, RoomName} ->
      IPString = Req:get(peer),
      case nexus:requestRoom(RoomName, IPString) of
        {ok, {RoomKey, RoomLong, RoomShort}, {UserKey, UserID}} ->
          Cookies = roomCookies(RoomKey, RoomLong, UserKey, UserID),
          tpaint_util:redirectTo(Req, "/room_" ++ RoomShort, Cookies);
        collision ->
          tpaint_util:textResponse(Req, "Hash collision; chances are good something's f'ed up.");
        {error, Description} ->
          tpaint_util:textResponse(Req, Description)
      end
  end.

handleEvents(Req) ->
  case Req:get_cookie_value("userKey") of
    undefined ->
      tpaint_util:respondJSON(Req, {array, []});
    KeyStr ->
      Key = list_to_atom(KeyStr),
      Events = userServer:getEvents(Key),
      tpaint_util:respondJSON(Req, {array, Events})
  end.

%% Internal API

get_option(Option, Options) ->
  {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
  ?assertEqual(
     "No, but I will!",
     "Have you written any tests?"),
  ok.

-endif.
