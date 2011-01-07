%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for tpaint.

-module(tpaint_web).
-author("Ben Ellis (benjaminster@gmail.com)").
-include("state.hrl").

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
          %room_<ShortID>
          %actual URL to be visited by user, displayed in address bar, etc.
          %looks up whether such a room exists,
          %checks cookies;
          %  if the room cookie is set for this room,
          %  assumes that the user cookie is valid for the room and just serves up the client page.
          %  if the room cookie is not set or is set for a different room,
          %  makes new cookies for this room and creates a new user in the room.
          %only place where cookies are set.
          "room_" ++ ShortID -> %room_<ShortID>
            case nexus:getRoomRef(ShortID) of
              error -> tpaint_util:textResponse(Req, "Sorry, no such room exists.");
              {ok, #roomRef{roomID = RoomID, roomPID = RoomPID}} ->
                case session:getSession(Req) of
                  {RoomID, _} -> %ignore whether the user ID is valid / registered for this room
                    Req:serve_file("doodle.html", DocRoot, []);
                  _ -> %ignore whether cookies set for other rooms
                    {_, UserID, Cookies} = session:makeUserSession(RoomID, Req:get(peer)),
                    case roomServer:addNewUser(RoomPID, UserID) of
                      ok ->
                        Req:serve_file("doodle.html", DocRoot, Cookies);
                      {error, Description} ->
                        tpaint_util:textResponse(Description)
                    end 
                end
            end;

          %state_<ShortID>
          %URL to be loaded by client via XHR
          %to be loaded on "entering" room or possibly on resyncing to the state of the room.
          %provides client with its view of the rooms current state
          "state_" ++ ShortID ->
            JSON = case getEstablishedSession(ShortID, Req) of
              {error, Why} ->
                tpaint_util:jsonError(Why);
              {ok, #roomRef{roomPID = PID}, #userRef{sessionID = UserID}} ->
                roomServer:getStateSeenBy(PID, UserID)
            end,
            tpaint_util:respondJSON(Req, JSON);

          %messages_<ShortID>
          %URL to be loaded by client via XHR
          %to be loaded on a polling basis
          %provides client with state change and other messages 
          "messages_" ++ ShortID ->
            JSON = case getEstablishedSession(ShortID, Req) of
              {error, Why} ->
                tpaint_util:jsonError(Why);
              {ok, _, #userRef{pid = PID}} ->
                {array, userServer:getMessages(PID, 5000)}
            end,
            tpaint_util:respondJSON(Req, JSON);
          _ ->
            Req:serve_file(Path, DocRoot)
        end;
      'POST' ->
        case Path of
          %makeRoom
          %URL to be posted to by the room creation page (or client, to sound fancy)
          %if the room name is present, hash it and the peer IP address to room and creator IDs
          %redirect to room_<ShortID> and set cookies on successful creation of room.
          "makeRoom" ->
            Params = dict:from_list(Req:parse_post()),
            case dict:find("roomName", Params) of
              error ->
                tpaint_util:redirectTo(Req, "/createRoom.html", []);
              {ok, RoomName} ->
                {RoomID, CreatorID, Cookies} = session:makeCreatorSession(RoomName, Req:get(peer)),
                case nexus:makeRoom(RoomID, CreatorID, RoomName) of
                  {ok, ShortID} -> tpaint_util:redirectTo(Req, "room_" ++ ShortID, Cookies);
                  collision -> tpaint_util:textResponse(Req, "Hash collision.");
                  {error, Description} -> tpaint_util:textResponse(Req, Description) 
                end
            end;
          %message
          %URL to be posted to by the client via XHR
          %posting to this URL is the app's RPC method; the posted data should be a JSON object
          %the posted JSON object must have at least kv pair, "what"/Method
          %Method must be a string.
          %Method might well be the key of another pair in the JSON object
          "message_" ++ ShortID ->
            JSONResponse = case rpcJSONParse(Req) of
              {error, Why} ->
                tpaint_util:jsonError(Why);
              {ok, Method, Params} ->
                case getEstablishedSession(ShortID, Req) of
                  {error, Why} ->
                    tpaint_util:jsonError(Why);
                  { ok, RoomRef, UserRef } ->
                    handleRPC(Method, Params, RoomRef, UserRef)
                end
            end, 
            tpaint_util:respondJSON(Req, JSONResponse);
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

% @spec getEstablishedSession( string(), request() ) -> {error, string()} | {ok, roomRef(), userRef()}
getEstablishedSession(ShortID, Req) ->
  case session:getSession(Req) of
    undefined ->  %missing at least one session cookie
      undefined;
    {RoomID, UserID} ->
      case string:str(RoomID, ShortID) of
        X when X /= 1 ->  %the short ID is not a prefix of the cookie room ID
          {error, "Cookie and URL do not match."};
        1 ->  %the short ID is a prefix of the room ID
          case nexus:getRoomRef(ShortID) of
            error ->
              {error, "No such room."};
            {ok, RoomRef = #roomRef{roomPID = RoomPID}} ->
              case roomServer:getUserRef(RoomPID, UserID) of
                error ->
                  {error, "No such user."};
                {ok, UserRef} ->
                  {ok, RoomRef, UserRef}
              end
          end
      end
  end.

rpcJSONParse(Req) ->
  JSONIn = mochijson:decode(Req:recv_body()),
  case JSONIn of
    {struct, Items} ->
      Params = dict:from_list(Items),
      case dict:find("what", Params) of
        error ->
          {error, "no method call"};
        {ok, Method} ->
          {ok, Method, Params}
      end;
    _ ->
      {error, "no method call"}
  end.
      
handleRPC("chat", Params, #roomRef{roomPID = RPID}, #userRef{pid = UPID, sessionID = UserID}) ->
  case dict:find("chat", Params) of
    error ->
      tpaint_util:jsonError("bad call to chat method");
    {ok, Message} ->
      Data = {struct, [{what, chat}, {chat, Message}, {from, userServer:getName(UPID)}]},
      roomServer:chatMessage(RPID, UserID, Data),
      "ok"
  end;
        
handleRPC("name", Params, #roomRef{roomPID = RPID}, #userRef{pid = UPID, sessionID = UserID}) ->
  case dict:find("name", Params) of
    error ->
      tpaint_util:jsonError("bad call to name method");
    {ok, Name} ->
      case userServer:setName(UPID, Name) of
        ok ->
          roomServer:nameWasSet(RPID, UserID, Name),
          {struct, [{name, Name},{status, "ok"}]};
        alreadySet ->
          "alreadySet";
        {error, Why} ->
          tpaint_util:jsonError(Why)
      end
  end;

handleRPC(_UnknownMethod, _, _, _) ->
  tpaint_util:jsonError("unknown method " ++ _UnknownMethod).
