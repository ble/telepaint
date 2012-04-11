-module(room).
-include("room.hrl").

-behaviour(gen_server).

-export([
    start_link/1,
    shutdown/1,
    add_observer/1,
    name_observer/3,
    game_action/3,
    get_observers/1,
    get_observer/2,
    has_observer/2,
    get_state/1,
    send_to_all/2,
    allow_anonymous_join/1
  ]).

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

game_action(Pid, ObserverId, Action) ->
  gen_server:call(Pid, {game_action, ObserverId, Action}).

get_observers(Pid) ->
  gen_server:call(Pid, get_observers).

get_observer(Pid, ObserverId) ->
  gen_server:call(Pid, {get_observer, ObserverId}).

join_game(Pid, PlayerId) ->
  gen_server:call(Pid, {join_game, PlayerId}).

has_observer(Pid, PlayerId) ->
  gen_server:call(Pid, {has_observer, PlayerId}).

get_state(Pid) ->
  gen_server:call(Pid, get_state).

send_to_all(Pid, Message) ->
  gen_server:call(Pid, {send_to_all, Message}).

%TODO: actually make this something determined by the room,
%      set by the creator, etc.
allow_anonymous_join(_Pid) ->
  true.

init(State) ->
  {ok, State}.

handle_call({join_game, ObserverId}, _, State0) ->
  case room_state:get_observer(State0, ObserverId) of
    {error, Reason} -> {reply, {error, Reason}, State0};
    {ok, Player} ->
      Game0 = State0#room.game,
      case game_state:add_player(Game0, Player) of
        {error, Reason} -> {reply, {disallowed, Reason}, State0};
        {ok, Msgs, Game1} ->
          internal_send_to_all(State0, Msgs),
          {reply, ok, State0#room{game = Game1}}
      end
  end;
  

handle_call(get_observers, _, State) ->
  {reply, {ok, room_state:get_observers(State)}, State};

handle_call({get_observer, ObserverId}, _, State) ->
  {reply, room_state:get_observer(State, ObserverId), State};

handle_call(add_observer, _, State0) ->
  {ok, {State1, Id, Msgs}} = room_state:add_observer(State0),
  {ok, QPid} = player_queue:start_link(),
  {ok, State2} = room_state:bind_observer(State1, Id, QPid),
  internal_send_to_all(State2, Msgs),
  {reply, {ok, Id}, State2};

handle_call({name_observer, Id, Name}, _, State0) ->
  {ok, {State1, Tag, Msgs}} = room_state:name_observer(State0, Id, Name),
  case Tag of
    set ->
      internal_send_to_all(State1, Msgs),
      {reply, ok, State1};
    rename ->
      {reply, {error, no_rename}, State0}
  end;

handle_call({game_action, Id, Action}, _, State0) ->
  io:format("GAME ON~n",[]),
%  {reply, ok, State0};
  case room_state:game_action(Id, Action, State0) of
    {ok, {State1, Msgs}} ->
      internal_send_to_all(State1, Msgs),
      {reply, ok, State1};
    {error, Reason} ->
      {reply, {error, Reason}, State0}
  end;

handle_call({has_observer, PlayerId}, _, State) ->
  Observers = room_state:get_observers(State),
  case [Observer || Observer <- Observers, Observer#player.id == PlayerId] of
    [] -> {reply, {ok, false}, State};
    _ -> {reply, {ok, true}, State}
  end;

handle_call(get_state, _, State) ->
  {reply, {ok, now(), State}, State};

handle_call({send_to_all, Message}, _, State) ->
  internal_send_to_all(State, [Message]),
  {reply, ok, State};

handle_call(shutdown, _, State) ->
  {stop, normal, ok, State}.

terminate(Reason, _State) ->
  io:format("~p stopping: ~p\n", [?MODULE, Reason]).

internal_send_to_all(State, Msgs) ->
  [player_queue:enqueue(Pid, Msgs) ||
    Player <- State#room.observers,
    Pid <- [Player#player.pid]].





