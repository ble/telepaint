-module(player_queue).
-behaviour(gen_fsm).

-export([start/0, last_poll_age/1, poll_after/2, enqueue/2]).
-export([handle_info/3, handle_event/3, init/1, terminate/3]).
-export([waiting/2]).

-compile(export_all).
-define(CULL_INTERVAL, 500).
-define(CULL_AGE, 500).
-define(WAIT_AGE, 150).
-define(E6, 1000000).
-define(E3, 1000).

start() ->
  gen_fsm:start(?MODULE, [], []).

last_poll_age(Pid) ->
  gen_fsm:sync_send_all_state_event(Pid, last_poll_age).

poll_after(Pid, After) ->
  gen_fsm:send_all_state_event(Pid, {poll_after, self(), After}),
  receive
    {Pid, Msgs} ->
      Msgs
  after 2 * ?WAIT_AGE ->
      []
  end.

enqueue(Pid, Msgs) ->
  gen_fsm:send_all_state_event(Pid, {enqueue, Msgs}).

%start:
%  spawn new queue process, return {ok, Pid}

%poll_age:
%  return the time of the most recent poll

%poll_after:
%  "waiting messages" are newer than the poll time
%  if no waiting messages:
%    add {new reference, poll time, polling pid} to the poll list
%    send a delayed message to myself {ref, polling pid}
%  if waiting messages:
%    send the waiting messages to the polling pid

%enqueue:
%  remove messages older than the cull age from the message list
%  add [{now, message} || message <- messages] to the message list
%  for each polling pid in the poll list:
%    if any messages are after the poll time
%      remove the polling pid from the list
%      send the messages to the pid

%%%%%%%%%%%%%%%%%%%
%timing enforcement
%%%%%%%%%%%%%%%%%%%
-type timestamp() :: {integer(), integer(), integer()}.

-record(pqs,
  {msgs = [] :: [{timestamp(), term()}],
   polling = [] :: [{{reference(), pid()}, timestamp()}],
   poll_time = {0,0,0} :: timestamp()}).


init(_Args) ->
  {ok, waiting, #pqs{}}.

terminate(_, _, _) ->
  ok.

waiting(timeout, State0) ->
  State1 = cull_messages(State0),
  case any_messages(State1) of
    [] ->
      {next_state, waiting, State1};
    _ ->
      {next_state, waiting, State1, ?CULL_INTERVAL}
  end.

handle_event({poll_after, PollPid, After}, StateName, State0) ->
  {_, State1} = add_poll(State0, PollPid, After),
  case any_messages(State1) of
    true ->
      {next_state, StateName, State1, ?CULL_INTERVAL};
    _ ->
      {next_state, StateName, State1}
  end;


handle_event({enqueue, Msgs}, StateName, State0) ->
  {next_state, StateName, enqueue_messages(State0, Msgs), ?CULL_INTERVAL}.

handle_info({R, P}, StateName, State0) when is_reference(R) and is_pid(P) ->
  State1 = cull_messages(State0),
  State2 = poll_timeout(State1, {R, P}),
  case any_messages(State2) of
      true ->
        {next_state, StateName, State2, ?CULL_INTERVAL};
      _ ->
        {next_state, StateName, State2}
  end.
 
any_messages(State) ->
  case State#pqs.msgs of
    [] ->
      false;
    _ ->
      true
  end.


%polling expiration:
%when the delayed polling message is received,
%  if the {ref, polling pid} is still in the poll list
%    remove it
%    send an empty message to the polling pid 
poll_timeout(State0 = #pqs{polling = Poll0}, Key = {R, P}) ->
  Key = {R, P},
  case proplists:get_value(Key, Poll0) of
    undefined ->
      State0;
    _ ->
      io:format("Poll ~p timed out.~n", [Key]), 
      P ! {self(), []},
      Poll1 = proplists:delete(Key, Poll0),
      State0#pqs{polling=Poll1}
  end.

cull_messages(State0 = #pqs{msgs = Msgs0}) ->
  Oldest = ago(?CULL_AGE),
  Msgs1 = [{X, T} || {X, T} <- Msgs0, later(T, Oldest)],
  io:format("Cull before ~p after ~p~n", [length(Msgs0), length(Msgs1)]),
  State0#pqs{msgs = Msgs1}.

enqueue_messages(State0 = #pqs{msgs = Msgs0}, NewMsgs) ->
  TNow = now(),

  %pick which polling processes will receive the messages
  {PollNotify, PollRetain} = lists:partition(
    fun({_, T}) ->
      later(TNow, T)
    end,
    State0#pqs.polling),

  %send them the messages
  lists:map(
    fun({{R, P}, _}) ->
      io:format("Poll ~p received message.~n", [{R, P}]),
      P ! {self(), NewMsgs}
    end,
    PollNotify),

  %remove the processes which have received messages
  %add the messages enqueued
  StampedMsgs = [{X, TNow} || X <- NewMsgs],
  State0#pqs{polling = PollRetain, msgs = StampedMsgs ++ Msgs0}.

%add polling process
%send delayed timeout to self
add_poll(State0 = #pqs{msgs = [], polling = Poll0}, Pid, After) ->
  Ref = make_ref(),
  PollItem = {{Ref, Pid}, After},
  Poll1 = [PollItem | Poll0],
  erlang:send_after(?WAIT_AGE, self(), {Ref, Pid}),
  {poll_wait, State0#pqs{polling = Poll1}};

add_poll(State0 = #pqs{msgs = Msgs, polling = Poll0}, Pid, After) ->
  MsgSend = lists:filter(
    fun({_, T}) ->
      later(T, After)
    end,
    Msgs),
  case MsgSend of
    [] ->
      PollItem = {{make_ref(), Pid}, After},
      Poll1 = [PollItem | Poll0],
      {poll_wait, State0#pqs{polling = Poll1}};
    _ ->
      Pid ! {self(), [X || {X, _} <- MsgSend]},
      {poll_sent, State0}
  end.

ago(Millis) ->
  {Mega, Unit, Micro} = now(),
  T0 = Micro + ?E6 * (Unit + ?E6 * Mega),
  T = T0 - Millis * ?E3,
  U0 = T div ?E6,
  M = U0 div ?E6,
  {M, U0 rem ?E6, T rem ?E6}.

later(X = {_A, _B, _C}, Y = {_D, _E, _F}) ->
  X > Y.
