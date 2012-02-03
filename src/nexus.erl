-module(nexus).

-behaviour(gen_server).

-export([start_link/1, shutdown/0, register_room/2, lookup_room/2, report/1]).

-export([init/1, handle_call/3, terminate/2]).

-record(context, {n = 0, list = []}).
start_link(Number) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Number, []).

shutdown() ->
  gen_server:call(?MODULE, shutdown).

register_room(RoomPid, RoomId) ->
  gen_server:call(?MODULE, {register_room, RoomPid, RoomId}).

lookup_room(Pid, Id) ->
  gen_server:call(Pid, {lookup_room, Id}).

report(Pid) ->
  gen_server:call(Pid, report).

init(Number) ->
  {ok, #context{n = Number}}.

handle_call(shutdown, _, State) ->
  {stop, normal, ok, State};

handle_call({register_room, Pid, Id}, _, State = #context{list = Lst, n = N}) ->
  L = length(Lst),
  if
    L < N ->
      Lst1 = [{Id, Pid} | Lst],
      {reply, ok, State#context{list = Lst1}};
    true ->
      {reply, {error, none_left}, State}
  end;


handle_call({lookup_room, Id}, _, State = #context{list = Lst}) ->
  case proplists:get_value(Id, Lst) of
    undefined ->
      {reply, {error, none_such}, State};
    Pid ->
      {reply, {ok, Pid}, State}
  end;

handle_call(report, _, State) ->
  {reply, {ok, State}, State}.

terminate(Reason, _State) ->
  io:format("~p stopping: ~p\n", [?MODULE, Reason]).
