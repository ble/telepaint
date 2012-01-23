-module(nexus).

-behaviour(gen_server).

-export([start_link/1, shutdown/1, make_room/2, lookup_room/2]).

-export([init/1, handle_call/3, terminate/2]).

-record(context, {n = 0, list = 0}).
start_link(Number) ->
  gen_server:start_link(?MODULE, Number, []).

shutdown(Pid) ->
  gen_server:call(Pid, shutdown).

make_room(Pid, Name) ->
  gen_server:call(Pid, {make_room, Name}).

lookup_room(Pid, Id) ->
  gen_server:call(Pid, {lookup_room, Id}).


init(Number) ->
  {ok, #context{n = Number}}.

handle_call(shutdown, _, State) ->
  {stop, normal, ok, State};

handle_call({make_room, Name}, _, State = #context{n = Number, list = Lst0}) ->
  if
    Number > 0 ->
      {ok, RoomPid, RoomId} = room:start_link(Name),
      Lst1 = [{RoomId, RoomPid} | Lst0],
      {reply, {ok, RoomId}, #context{n = Number - 1, list = Lst1}};
    true ->
      {reply, {error, none_left}, State}
  end;

handle_call({lookup_room, Id}, _, State = #context{list = Lst}) ->
  case proplists:get_value(Id, Lst) of
    undefined ->
      {reply, {error, none_such}, State};
    Pid ->
      {reply, {ok, Pid}, State}
  end.

terminate(Reason, _State) ->
  io:format("~p stopping: ~p\n", [?MODULE, Reason]).
