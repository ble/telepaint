-module(room).
-include("room.hrl").

-behaviour(gen_server).

-export([
    start_link/1,
    shutdown/1,
    add_observer/1,
    name_observer/3,
    get_observers/1,
    has_observer/2]).

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

has_observer(Pid, PlayerId) ->
  gen_server:call(Pid, {has_observer, PlayerId}).

init(State) ->
  {ok, State}.

handle_call({join_game, ObserverId}, _, State0) ->
  case get_observer(State0, ObserverId) of
    undefined -> {reply, {error, none_such}, State0};
    {error, Reason} -> {reply, {error, Reason}, State0};
    {ok, Player} ->
      Game0 = State0#room.game,
      case game_state:add_player(Game0, Player) of
        {error, Reason} -> {reply, {disallowed, Reason}, State0};
        {ok, Msgs, Game1} ->
          send_to_all(State0, Msgs),
          {reply, ok, State0#room{game = Game1}}
      end
  end;
  

handle_call(get_observers, _, State) ->
  {reply, room_state:get_observers(State), State};

handle_call(add_observer, _, State0) ->
  {ok, {State1, Id, Msgs}} = room_state:add_observer(State0),
  {ok, QPid} = player_queue:start_link(),
  {ok, State2} = room_state:bind_observer(State1, Id, QPid),
  send_to_all(State2, Msgs),
  {reply, {ok, Id}, State2};

handle_call({name_observer, Id, Name}, _, State0) ->
  {ok, {State1, Tag, Msgs}} = room_state:name_observer(State0, Id, Name),
  case Tag of
    set ->
      send_to_all(State1, Msgs),
      {reply, ok, State1};
    rename ->
      {reply, {error, no_rename}, State0}
  end;

handle_call({has_observer, PlayerId}, _, State) ->
  {ok, Observers} = room_state:get_observers(State),
  case [Observer || Observer <- Observers, Observer#player.id == PlayerId] of
    [] -> {reply, {ok, false}, State};
    _ -> {reply, {ok, true}, State}
  end;

handle_call(shutdown, _, State) ->
  {stop, normal, ok, State}.

terminate(Reason, _State) ->
  io:format("~p stopping: ~p\n", [?MODULE, Reason]).

get_observer(State, Id) ->
  case [Player || Player <- State#room.observers, Player#player.id == Id] of
    [] -> undefined;
    [P] -> {ok, P};
    _ -> {error, duplicate}
  end.

send_to_all(State, Msgs) ->
  [player_queue:enqueue(Pid, Msgs) ||
    Player <- State#room.observers,
    Pid <- [Player#player.pid]].





