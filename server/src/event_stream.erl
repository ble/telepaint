-module(event_stream).
-behaviour(gen_event).

%%% External API- creating and managing streams.
-export([start_container/0, stop_container/1, add_stream/3, delete_stream/2]).
-export([add_stream_handler/3, remove_stream_handler/3]).

%%% External API- sending events / getting messages
-export([get_stream_messages/4, send_event/2]).

%%% Time until messages in the queue go stale
-define(STALE_TIME_MICROSECONDS, 30 * 1000 * 1000). 

%%% Behaviour exports
-export([init/1, terminate/2, code_change/3]).
-export([handle_event/2, handle_call/2, handle_info/2]).

%%% Test export
-export([test/0]).
test() ->
  {ok, Pid} = event_stream:start_container(),
  Id = foobar,
  A = event_stream:add_stream(Pid, Id, [fun(I, E) -> {ok, [{I, E}]} end]),
  B = event_stream:send_event(Pid, <<"hello there, friend">>),
  C = event_stream:get_stream_messages(Pid, Id, {0,0,0}, 500),
  T1 = case C of
    {T, _} ->
      T;
    _ ->
      {0, 0, 0}
  end,
  D = event_stream:get_stream_messages(Pid, Id, T1, 500),
  E = event_stream:send_event(Pid, <<"howdang">>),
  F = event_stream:get_stream_messages(Pid, Id, T1, 500),
  G = event_stream:stop_container(Pid),
  {A,B,C,D,E,F,G}.


%%%' Creating and managing streams
start_container() ->
  gen_event:start().

stop_container(Pid) ->
  gen_event:stop(Pid).

add_stream(Pid, Id, Handlers) ->
  gen_event:add_handler(Pid, {event_stream, Id}, [Id, Handlers]).

delete_stream(Pid, Id) ->
  gen_event:delete_handler(Pid, {event_stream, Id}, no_args).

add_stream_handler(Pid, Id, Handler) ->
  gen_event:call(Pid, {event_stream, Id}, {attach_handler, Handler}).

remove_stream_handler(Pid, Id, Handler) ->
  gen_event:call(Pid, {event_stream, Id}, {remove_handler, Handler}).
%%%.
%%%' Sending events / getting messages
get_stream_messages(Pid, Id, LastReqTime, Timeout) ->
  Handler = {event_stream, Id},
  Ref = make_ref(),
  parked = gen_event:call(Pid, Handler, {park, self(), Ref, LastReqTime}),
  receive
    {event_stream, Ref, Messages} ->
      LastTime = lists:max([Time || {Time, _} <- Messages]),
      MsgBodies = [Body || {_, Body} <- Messages],
      {LastTime, MsgBodies}
  after
    Timeout ->
      timeout
  end.

send_event(Pid, Event) ->
  gen_event:notify(Pid, Event).
%%%.
%%%' Internal definitions
-record(
  stream_state,
  { stream_id = undefined,
    handlers = [],
    parked_requests = [],
    messages = queue:new()}).
%%%.

%%%' Control callbacks
init([StreamId, Handlers]) ->
  State0 = make_state(StreamId),
  State1 = attach_handlers(Handlers, State0),
  {ok, State1}.

terminate(_Arg, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
%%%.  
%%%' Message callbask
handle_event(Event, State0) ->
  State1 = discard_stale(State0),
  State2 = process_handlers(Event, State1),
  State3 = dispatch_responses(State2),
  {ok, State3}.

handle_call({attach_handler, Handler}, State = #stream_state{handlers = Handlers0}) ->
  Handlers1 = case lists:member(Handler, Handlers0) of
    true -> Handlers0;
    false -> [Handler | Handlers0]
  end,
  {ok, attached, State#stream_state{handlers = Handlers1}};

handle_call({remove_handler, Handler}, State = #stream_state{handlers = Handlers0}) ->
  Handlers1 = lists:delete(Handler, Handlers0),
  Reply = case lists:member(Handler, Handlers0) of
    true -> removed;
    false -> absent
  end,
  {ok, Reply, State#stream_state{handlers = Handlers1}};


handle_call({park, Pid, Ref, LastInstant}, State0 = #stream_state{parked_requests = Requests0}) ->
  Requests1 = [{LastInstant, Pid, Ref} | Requests0], 
  State1 = discard_stale(State0),
  State2 = State1#stream_state{parked_requests = Requests1},
  State3 = dispatch_responses(State2),
  {ok, parked, State3}.

handle_info(_Msg, State) ->
  {ok, State}.

%%%.
%%%' Private functions
make_state(StreamId) ->
  #stream_state{stream_id=StreamId}.


attach_handlers([], State) ->
  State;

attach_handlers([Handler | Handlers], State0 = #stream_state{handlers = Current}) ->
  case lists:member(Handler, Current) of
    true ->
      attach_handlers(Handlers, State0);
    false ->
      attach_handlers(Handlers, State0#stream_state{handlers = [Handler | Current]})
  end.

discard_stale(State = #stream_state{messages = Messages0}) ->
  Instant = now(),
  Messages1 = queue:filter(
    fun({Stamp, _}) -> timer:now_diff(Instant, Stamp) < ?STALE_TIME_MICROSECONDS end,
    Messages0),
  State#stream_state{messages = Messages1}.

process_handlers(Event, State = #stream_state{stream_id = Id, messages = Messages0, handlers = Handlers0}) ->
  {NewMessages0, Handlers1} = process_message_generation(Id, Event, Handlers0),
  Instant = now(),
  NewMessages1 = [{Instant, Message} || Message <- NewMessages0],
  Messages1 = queue:join(Messages0, queue:from_list(NewMessages1)),
  State#stream_state{messages = Messages1, handlers = Handlers1}.


process_message_generation(Id, Event, Handlers) ->
  process_message_generation({[], []}, Id, Event, Handlers).

process_message_generation(X, _, _, []) ->
  X;

process_message_generation({Ms0, Hs0}, Id, Event, [Handler | Handlers]) ->
  {Status, Messages} = Handler(Id, Event),
%  {Status, Messages} = Handler:handle_event(Id, Event),
  Ms1 = Messages ++ Ms0,
  Hs1 = case Status of
    remove_handler -> Hs0;
    ok -> [Handler | Hs0]
  end,
  process_message_generation({Ms1, Hs1}, Id, Event, Handlers).

dispatch_responses(State = #stream_state{messages = Messages, parked_requests = Requests0}) ->  
  Requests1 = dispatch_responses(Messages, Requests0, []),
  State#stream_state{parked_requests = Requests1}.

dispatch_responses(Messages, [Req = {Stamp, Pid, Ref} | Requests], ReqRemaining) ->
  MessagesToSend = queue:to_list(messages_after(Stamp, Messages)),
  case MessagesToSend of
    [] ->
      dispatch_responses(Messages, Requests, [Req | ReqRemaining]);
    _ ->
      Pid ! {event_stream, Ref, MessagesToSend},
      dispatch_responses(Messages, Requests, ReqRemaining)
  end;

dispatch_responses(_, [], ReqRemaining) ->
  ReqRemaining.

messages_after(Cutoff, Queue) ->
  queue:filter(fun({Stamp, _}) -> timer:now_diff(Stamp, Cutoff) > 0 end, Queue).
%%%.
%%% vim: set filetype=erlang tabstop=2 foldmarker=%%%',%%%. foldmethod=marker:
