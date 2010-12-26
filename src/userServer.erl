-module(userServer).
-include_lib("kernel/include/file.hrl").
-behaviour(gen_server).


-export([start_link/3, shutdown/1, setRoomKey/2, getEvents/1, sendMessage/2, savePicture/3]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(state, {key, uid, roomName, roomKey,
                userName, jsonMailbox, pollingPid}).

init_state(Key, UID, RoomName) -> #state{
  key=Key,
  uid=UID,
  roomName=RoomName,
  roomKey=undefined,
  userName=undefined,
  jsonMailbox=[],
  pollingPid=none}.

set_poll(State, PID) -> State#state{pollingPid=PID}.
clear_mailbox(State) -> State#state{jsonMailbox=[]}.


enqueue_message(State = #state{jsonMailbox=Mailbox}, Message) ->
  NewState = State#state{jsonMailbox=[Message | Mailbox]},
  Pumped = pump_messages(NewState),
  Pumped.

pump_messages(State = #state{jsonMailbox=Mailbox, pollingPid=PID}) ->
  tpaint_util:pr("pumping messages..."),
  Live = is_pid(PID) andalso is_process_alive(PID),
  if
    not Live ->
      State;
    Live ->
      tpaint_util:pr("sending messages..."),
      PID ! {messages, self(), lists:reverse(Mailbox)}, 
      clear_mailbox(set_poll(State, undefined))
  end.


debug_id(#state{key=Key, uid=UID, userName=UserName}) ->
  lists:flatten(io_lib:fwrite("{~p: ~p ~p ~p}", [self(), Key, UserName, UID])). 

start_link(Key, UID, Name) ->
  case gen_server:start_link(?MODULE, init_state(Key, UID, Name), []) of
    {ok, PID} ->
      {ok, PID};
%      case catch register(Key, PID) of
%        true -> {ok, Key};
%        false -> error
%      end;
    _ ->
      error
  end.

init(State) ->
  {ok, State}.

replyError(Format, ExtraArgs, State) ->
  Desc = tpaint_util:sfmt("~p: " ++ Format, [?MODULE | ExtraArgs]),
  tpaint_util:pr(Desc),
  {reply, {error, Desc}, State}.

getDirectory(#state{uid=UID}) ->
  "dyn/" ++ UID.

getExtension("data:image/png;base64," ++ _) ->
  ".png".

decodePicture("data:image/png;base64," ++ Data) ->
  base64:decode(Data).


savePicture(ServerRef, UserIP, PicData) ->
  gen_server:call(ServerRef, {savePicture, UserIP, PicData}).

getEvents(Key) ->
  gen_server:cast(Key, {getEvents, self()}),
  PID = whereis(Key),
  receive
    {messages, PID, Messages} ->
      Messages
  after
    7500 ->
      []
  end.

sendMessage(ServerRef, JSO) ->
  tpaint_util:pr(JSO),
  gen_server:call(ServerRef, {sendMessage, JSO}).

setRoomKey(ServerRef, RoomKey) ->
  gen_server:call(ServerRef, {setRoomKey, RoomKey}).

shutdown(ServerRef) ->
  ok = gen_server:call(ServerRef, shutdown).

handle_call({savePicture, UserIP, PicData}, _, State = #state{uid=UID}) ->
  Directory = getDirectory(State),
  {Status, Info} = file:read_file_info(Directory),
  ReadyToWrite =
    case Status of
      error ->
        file:make_dir(Directory);
      ok ->
        case Info#file_info.type of
          directory -> ok;
          _ -> error
        end;
      _ -> error
    end,
        
%  ReadyToWrite = case Info of
%    {error, enoent} -> file:make_dir(Directory);
%    #file_info{type=Directory} -> ok;
%    Other -> Other
%  end,
  tpaint_util:pr(ReadyToWrite),
  tpaint_util:pr(Directory),
  case ReadyToWrite of
    ok ->
      Extension = getExtension(PicData),
      Filename = tpaint_util:getPicUID(UID, UserIP, string:substr(PicData, 1, 256)) ++ Extension,
      Path = Directory ++ "/" ++ Filename,
      Binary = decodePicture(PicData), 
      case file:write_file(Path, Binary) of
        ok -> {reply, {ok, Path}, State};
        _ -> {reply, error, State}
      end;
    _ -> {reply, error, State}
  end;



handle_call({setRoomKey, RoomKey}, _, State = #state{roomKey=undefined, key=UserKey}) ->
  roomServer:addUser(RoomKey, UserKey),
  {reply, ok, State#state{roomKey=RoomKey}};

handle_call({setRoomKey, _}, _, State) ->
  replyError("room key already set.", [], State);

handle_call(shutdown, _, State) ->
  {stop, normal, ok, State};

handle_call({sendMessage, JSO}, _, State) ->
  tpaint_util:pr("Enqueueing message."),
  {reply, ok, enqueue_message(State, JSO)};

handle_call(Call, _, State) ->
  io:format("~p: ~p reports unknown synchronous call ~p", [?MODULE, debug_id(State), Call]),
  {reply, {unknownCall, Call}, State}.

handle_cast({getEvents, From}, State = #state{jsonMailbox=Mailbox}) ->
  if
    length(Mailbox) > 0 ->
      {noreply, pump_messages(State#state{pollingPid = From})};
    true ->
      {noreply, State#state{pollingPid = From}}
  end;

handle_cast(Cast, State) ->
  io:format("~p: ~p reports unknown cast ~p", [?MODULE, debug_id(State), Cast]),
  {noreply, State}.

handle_info(OOB, State) ->
  io:format("~p: ~p reports unknown oob message ~p", [?MODULE, debug_id(State), OOB]),
  {noreply, State}.

terminate(Reason, State) ->
  io:format("~p:~p stopping: ~p\n", [?MODULE, debug_id(State), Reason]).

code_change(_, State, _) ->
  {ok, State}.
