-module(userServer).
-include_lib("kernel/include/file.hrl").
-include_lib("state.hrl").
-behaviour(gen_server).


-export([start/2, shutdown/1, setName/2, getName/1, getMessages/2, enqueue/2, beginGame/1]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

debug_id(#user{sessionID=SessionID, roomID=RoomID, name=name}) ->
  lists:flatten(io_lib:fwrite("{~p: ~p ~p ~p}", [self(), SessionID, name, RoomID])). 

start(UserID, RoomID) ->
  case gen_server:start(?MODULE, {UserID, RoomID}, []) of
    {ok, PID} -> {ok, PID};
    _ -> error
  end.

init({UserID, RoomID}) -> {
    ok,
    #user{
      sessionID = UserID,
      roomID = RoomID,
      name = undefined,
      pid = self(),
      mailbox = [],
      pollingPIDs = []
    }}.


getName(PID) ->
  gen_server:call(PID, getName).

setName(PID, Name) ->
  gen_server:call(PID, {setName, Name}).

getMessages(PID, Timeout) ->
  gen_server:cast(PID, {getMessages, self()}),
  receive
    {messages, PID, Messages} -> Messages
  after
    Timeout -> []
  end.

enqueue(PID, Message) ->
  gen_server:call(PID, {enqueue, Message}).

beginGame(PID) ->
  gen_server:call(PID, beginGame).

shutdown(ServerRef) ->
  ok = gen_server:call(ServerRef, shutdown).

handle_call(getName, _, State = #user{name=Name}) ->
  {reply, Name, State};

handle_call({setName, Name}, _, State = #user{name=undefined}) ->
  {reply, ok, State#user{name=Name}};

handle_call({setName, _Name}, _, State = #user{name=_AlreadySet}) ->
  {reply, alreadySet, State};

handle_call({enqueue, Message}, _, State) ->
  case enqueueMessage(Message, State) of
    {ok, NewState} ->
      {reply, ok, NewState};
    _ ->
      {reply, {error, "Couldn't enqueue message"}}
  end;

handle_call(shutdown, _, State) ->
  {stop, normal, ok, State};

handle_call(beginGame, _, State) ->
  case enqueueMessage({struct, [{method, beginGame}]}, State) of
    {ok, NewState} ->
      {reply, ok, NewState};
    _ ->
      {reply, error, State}
  end;


handle_call(Call, _, State) ->
  io:format("~p: ~p reports unknown synchronous call ~p", [?MODULE, debug_id(State), Call]),

  {reply, {unknownCall, Call}, State}.

handle_cast({getMessages, From}, State = #user{mailbox = Mailbox, pollingPIDs = PIDs}) ->
  case Mailbox of
    [] ->
      {noreply, State#user{pollingPIDs = [From | PIDs]}};
    _ ->
      lists:foreach(
        fun (PID) -> PID ! {messages, self(), Mailbox} end,
        [From | PIDs]),
      {noreply, State#user{mailbox = [], pollingPIDs = []}}
  end;

handle_cast(Cast, State) ->
  io:format("~p: ~p reports unknown cast ~p", [?MODULE, debug_id(State), Cast]),
  {noreply, State}.

handle_info(OOB, State) ->
  io:format("~p: ~p reports unknown oob message ~p", [?MODULE, debug_id(State), OOB]),
  {noreply, State}.

%for internal use
enqueueMessage(Message, State = #user{mailbox = Mailbox, pollingPIDs = PIDs}) ->
  Messages = [Message|Mailbox],
  case PIDs of
    [] ->
      {ok, State#user{mailbox = Messages}};
    _ ->
      [PID ! {messages, self(), lists:reverse(Messages)} || PID <- PIDs],
      {ok, State#user{mailbox = [], pollingPIDs = []}}
  end.



terminate(Reason, State) ->
  io:format("~p:~p stopping: ~p\n", [?MODULE, debug_id(State), Reason]).

code_change(_, State, _) ->
  {ok, State}.
