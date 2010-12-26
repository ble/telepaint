-module(roomServer).
-include_lib("kernel/include/file.hrl").
-behaviour(gen_server).


-export([start_link/4, shutdown/0, getName/1, stroke/2, strokesSince/2, strokeToJSON/1, addUser/2]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {key, uid, roomName, users, creator}).
-record(stroke, {time, coordinates}).

strokeToJSON(Term) ->
  io:format("~p~n", [Term]),
  strokeToJSON(Term, actual).

strokeToJSON(#stroke{time={Mega, Unit, Micro}, coordinates=Coordinates}, actual) ->
  {struct, [{"time", {array, [Mega, Unit, Micro]}}, {"coordinates", {array, Coordinates}}]}.

start_link(Key, UID, Name, {CreatorID, CreatorPID}) ->
  gen_server:start_link(
    ?MODULE,
    #state{
      key=Key,
      uid=UID,
      roomName=Name,
      users=gb_sets:new()},
    []).

shutdown() ->
  ok = gen_server:call(?MODULE, shutdown).

getName(ServerRef) ->
  gen_server:call(ServerRef, getName).

stroke(ServerRef, Coordinates) ->
  gen_server:call(ServerRef, {stroke, Coordinates}).

strokesSince(ServerRef, Time) ->
  gen_server:call(ServerRef, {strokesSince, Time}).

addUser(ServerRef, UserKey) ->
  gen_server:call(ServerRef, {addUser, UserKey}).

init(Arg) -> 
  {ok, Arg}.

getDirectory(#state{uid=UID}) ->
  "dyn/" ++ UID.

getExtension("data:image/png;base64," ++ _) ->
  ".png".

decodePicture("data:image/png;base64," ++ Data) ->
  base64:decode(Data).

% handle_call(getNextKey, _, State = #state{free=Free}) ->
%   Next = gb_sets:next(gb_sets:iterator(Free)),
%   case Next of
%     {Key, _} ->
%       Remaining = gb_sets:del_element(Key, Free),
%       {reply, {ok, Key}, State#state{free=Remaining}};
%     none ->
%       {reply, noKey, State}
%   end;
% 
% handle_call(getNextKey, _, State) ->
%   {stop, {unexpectedState, State}, error, []};



handle_call(shutdown, _, State) ->
  {stop, normal, ok, State};

handle_call(getName, _, State = #state{roomName=Name}) ->
  {reply, Name, State};

handle_call({addUser, UserKey}, _, State = #state{users=Users}) ->
  {reply, ok, State#state{users=gb_sets:add_element(UserKey, Users)}};

% handle_call({stroke, Coordinates}, _, State=#state{strokes=Strokes}) ->
%   StrokeTime = now(),
%   Stroke = #stroke{time=StrokeTime, coordinates=Coordinates},
%   NewStrokes = [Stroke | Strokes],
%   {reply, {ok, StrokeTime, length(NewStrokes)}, State#state{strokes=NewStrokes}};
% 
% handle_call({strokesSince, never}, _, State=#state{strokes=Strokes}) ->
%   io:format("yes!~n"),
%   {reply, {Strokes, now()}, State};
% 
% handle_call({strokesSince, LastTime}, _, State=#state{strokes=Strokes}) ->
%   io:format("yes!~n"),
%   AfterLast =
%     fun (#stroke{time=Time}) ->
%       timer:now_diff(Time, LastTime) > 0
%     end,
%   UnseenStrokes = lists:takewhile(AfterLast, Strokes),
%   {reply, {UnseenStrokes, now()}, State};

handle_call({recordPic, userIP, picData}, _, State = #state{uid=UID}) ->
  Directory = getDirectory(State),
  Info = file:read_file_info(Directory),
  Type = Info#file_info.type,
  ReadyToWrite = case Type of
    directory -> ok;
    {error, enoent} -> file:make_dir(Directory);
    Other -> Other
  end,
  case ReadyToWrite of
    ok ->
      Extension = getExtension(picData),
      Filename = tpaint_util:getPicUID(UID, userIP, string:substr(picData, 1, 256)) ++ Extension,
      Path = Directory ++ "/" ++ Filename,
      Binary = decodePicture(picData), 
      case file:write_file(Path, Binary) of
        ok -> {reply, {ok, Path}, State};
        _ -> {reply, error, State}
      end;
    _ -> {reply, error, State}
  end;

handle_call(Request, _, State) ->
  {reply, {unknownRequest, Request}, State}.

% handle_cast({returnKey, Key}, State = #state{free=Free, all=All}) ->
%   case gb_sets:is_member(Key, All) of
%     true ->
%       {noreply, State#state{free=gb_sets:add_element(Key, Free)}};
%     false->
%       {noreply, State}
%   end;
  

handle_cast(_, State) ->
  io:format("~p: meaningless async call", [?MODULE]),
  {noreply, State}.

handle_info(_, State) ->
  io:format("~p: meaningless oob call", [?MODULE]),
  {noreply, State}.

terminate(Reason, _State) ->
  io:format("~p stopping: ~p\n", [?MODULE, Reason]).

code_change(_, State, _) ->
  {ok, State}.
