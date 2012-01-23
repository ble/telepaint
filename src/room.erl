-module(room).
-include("room.hrl").

-behaviour(gen_server).

-export([
    start_link/1,
    shutdown/1,
    add_observer/1,
    name_observer/3,
    get_observers/1]).

-export([
    join_game/2

  ]).

-export([init/1, handle_call/3, terminate/2]).

start_link(Name) ->
  {ok, State} = room_state:make(Name),
  {ok, Pid} = gen_server:start_link(?MODULE, State, []),
  {ok, Pid, State#room.id}.

shutdown(Pid) ->
  gen_server:call(Pid, shutdown).

add_observer(Pid) ->
  gen_server:call(Pid, add_observer).

name_observer(Pid, Id, Name) ->
  gen_server:call(Pid, {name_observer, Id, Name}).

get_observers(Pid) ->
  gen_server:call(Pid, get_observers).

join_game(Pid, PlayerId) ->
  gen_server:call(Pid, {join_game, PlayerId}).

init(State) ->
  {ok, State}.

handle_call(get_observers, _, State) ->
  {reply, room_state:get_observers(State), State};

handle_call(add_observer, _, State0) ->
  {ok, {State1, Id}} = room_state:add_observer(State0),
  {ok, QPid} = player_queue:start_link(),
  {ok, State2} = room_state:bind_observer(State1, Id, QPid),
  {reply, {ok, Id}, State2};

handle_call({name_observer, Id, Name}, _, State0) ->
  {ok, {Tag, State1}} = room_state:name_observer(State0, Id, Name),
  case Tag of
    set ->
      {reply, ok, State1};
    rename ->
      {reply, {error, no_rename}, State0}
  end;

handle_call(shutdown, _, State) ->
  {stop, normal, ok, State}.

terminate(Reason, _State) ->
  io:format("~p stopping: ~p\n", [?MODULE, Reason]).
