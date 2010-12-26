-module(nexus).
-behaviour(gen_server).

%%external api
-export([start_link/0, start_link/2, getRoomInfo/1, requestRoom/2, newUserIn/2, shutdown/0]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%for registration pools
-record(pool, {available, all}).

make_pool(Items) ->
  Set = gb_sets:from_list(Items),
  #pool{available=Set, all=Set}.

take_pool(Pool = #pool{available=Avail}) ->
  case gb_sets:is_empty(Avail) of
    true -> noneLeft;
    false ->
      {First, Rest} = gb_sets:take_smallest(Avail),
      {ok, First, Pool#pool{available=Rest}} 
  end.

return_pool(Pool = #pool{available=Avail, all=All}, Item) ->
  case gb_setsi:s_element(Item, All) of
    true ->
      Pool#pool{available=gb_sets:add_element(Item, Avail)};
    false ->
      Pool
  end.  

%%state manipulation
-record(state, {roomsByShortID, roomKeys, userKeys}).  
formatAtom(Format, Params) ->
  erlang:list_to_atom(lists:flatten(io_lib:fwrite(Format, Params))).

init_state(RoomCount, UserCount) ->
  MkRoomKey = fun (N) -> formatAtom("room~4.16.0B", [N]) end,
  MkUserKey = fun (N) -> formatAtom("user~4.16.0B", [N]) end,
  #state{
    roomsByShortID = dict:new(),
    roomKeys = make_pool(lists:map(MkRoomKey, lists:seq(0, RoomCount-1))),
    userKeys = make_pool(lists:map(MkUserKey, lists:seq(0, UserCount-1)))}.

%%external API
start_link() ->
  start_link(4, 32). %default limits

start_link(RoomCount, UserCount) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, init_state(RoomCount, UserCount), []).

requestRoom(RoomName, IPString) ->
  gen_server:call(?MODULE, {requestRoom, RoomName, IPString}).

getRoomInfo(ShortID) ->
  gen_server:call(?MODULE, {getRoomInfo, ShortID}).

newUserIn(ShortID, IP) ->
  gen_server:call(?MODULE, {newUserIn, ShortID, IP}).

shutdown() ->
  ok = gen_server:call(?MODULE, shutdown).

%%gen server callbacks
init(State) -> 
  {ok, State}.

handle_call(shutdown, _, State) ->
  {stop, normal, ok, State};

handle_call({getRoomInfo, ShortID}, _, State) ->
  '_getRoomInfo'(ShortID, State);
    
handle_call({requestRoom, Name, IP}, _, State) -> 
  '_requestRoom'(Name, IP, State);

handle_call({newUserIn, RoomShortID, IP}, _, State) ->
  '_newUserIn'(RoomShortID, IP, State); 

handle_call(Term, _, State) ->
  io:format("Unexpected call ~p\n", [Term]),
  {reply, error, State}.


%      case Instance:start_link() of
%        {ok, Pid} ->
%          case registrationServer:getNextKey() of
%            {ok, Key} ->
%              case catch register(Pid, Key) of
%                true ->
%                  Record = {Pid, Name, UID},
%                  State2 = dict:store(ShortID, Record),
%                  {reply, {ok, ShortID}, State2};
%                _ ->
%                  {reply, error, State}
%              end;
%            noKey ->
%              {reply, error, State}
%          end;
%        _ ->
%          {reply, error, State}
%      end
%  end.

handle_cast(_Request, State) ->
  io:format("~p: meaningless async call", [?MODULE]),
  {noreply, State}.

handle_info(_Info, State) ->
  io:format("~p: meaningless oob call", [?MODULE]),
  {noreply, State}.

terminate(Reason, _State) ->
  io:format("~p stopping: ~p\n", [?MODULE, Reason]).

code_change(_, State, _) ->
  {ok, State}.

replyError(Format, ExtraArgs, State) ->
  Desc = tpaint_util:sfmt("~p: "++ Format, [?MODULE | ExtraArgs]),
  tpaint_util:pr(Desc),
  {reply, {error, Desc}, State}.

'_newUserIn'(RoomShortID, IP, State) ->
  case '_getRoomInfo'(RoomShortID, State) of
    {reply, {ok, {RoomKey, RoomShortID, UID, _RoomName}}, State} ->
      case '_newUserIn'(RoomKey, UID, IP, State) of
        {reply, {ok, {UserKey, UserID}}, NewState} ->
          userServer:setRoomKey(UserKey, RoomKey),
          {reply, {ok, {{UserKey, UserID}, {RoomKey, UID}}}, NewState};
        Unsuccess ->
          Unsuccess
      end;
    _ ->
      replyError("invalid room", [], State)
  end.



'_newUserIn'(RoomKey, RoomUID, IP, State) -> 
  case '_newUser'(RoomUID, IP, State) of
    Reply = {reply, {ok, {UserKey, _UserID}}, _NewState} ->
      userServer:setRoomKey(UserKey, RoomKey),
      Reply;
    AnythingElse ->
      AnythingElse
  end.

'_newUser'(RoomUID, IP, State = #state{userKeys=UserKeys}) -> 
    UserID = tpaint_util:getUserUID(RoomUID, IP),
    case take_pool(UserKeys) of
      noneLeft ->
        replyError("out of user keys", [], State);
      {ok, UserKey, UserKeysRemaining} ->
        case userServer:start_link(UserKey, UserID, undefined) of
          {error, _} ->
            replyError("failed to start user server", [], State);
          {ok, UserPID} ->
            true = register(UserKey, UserPID),
            NewState = State#state{userKeys=UserKeysRemaining},
            {reply, {ok, {UserKey, UserID}}, NewState}
        end
    end.  

'_getRoomInfo'(ShortID, State = #state{roomsByShortID=RoomByID}) ->
    case dict:find(ShortID, RoomByID) of
      error ->
        replyError("unknown short room ID.", [], State);
      Result = {ok, {_RoomKey, _RoomShortID, _UID, _RoomName}} ->
        {reply, Result, State}
    end.

'_requestRoom'(RoomName, IP, State = #state{roomsByShortID=RoomByID, roomKeys=RoomKeys}) ->
  UID = tpaint_util:getRoomUID(RoomName, IP),
  ShortID = tpaint_util:shorten(UID),
  %% see if there's a hash collision
  case dict:is_key(ShortID, RoomByID) of
    true ->
      {reply, collision, State};
    false ->
      %% get room key
      case take_pool(RoomKeys) of
        noneLeft -> 
          replyError("no room keys left", [], State);
        {ok, RoomKey, RoomKeysRemaining} ->
          %% get user key
          case '_newUser'(UID, IP, State#state{roomKeys=RoomKeysRemaining}) of
            {reply, {error, Desc}, _} ->
              {reply, {error, Desc}, State};
            {reply, {ok, {UserKey, UserID}}, #state{userKeys=UserKeysRemaining}} ->
              case roomServer:start_link(RoomKey, UID, RoomName, {UserID, UserKey}) of
                {error, _} ->
                  userServer:shutdown(UserKey),
                  replyError("failed to start user process", [], State);
                {ok, RoomPID} ->
                  true = register(RoomKey, RoomPID),
                  userServer:setRoomKey(UserKey, RoomKey),
                  NewState = State#state{
                    roomsByShortID=dict:store(ShortID, {RoomKey, ShortID, UID, RoomName}, RoomByID),
                    roomKeys=RoomKeysRemaining,
                    userKeys=UserKeysRemaining},
                  {reply, {ok, {RoomKey, UID, ShortID}, {UserKey, UserID}}, NewState}
              end
          end
      end
  end.

%              
%          case take_pool(UserKeys) of
%            noneLeft -> {reply, error, State};
%            {ok, UserKey, UserKeysRemaining} ->
%              UserID = tpaint_util:getUserUID(UID, IP),
%              %% start user process
%              case userServer:start_link(UserKey, UserID, undefined) of
%                {error, _} -> {reply, error, State};
%                {ok, UserPID} ->
%                  %% start room process
%                  case roomServer:start_link(RoomKey, UID, Name, {UserID, UserPID}) of
%                    {error, _} ->
%                      userServer:shutdown(UserPID),
%                      {reply, error, State};
%                    {ok, RoomPID} ->
%                      %%register user and room processes
%                      %%note absence of error handling here.
%                      io:format("~p~n~p~n", [UserPID, RoomPID]),
%                      true = register(UserKey, UserPID),
%                      true = register(RoomKey, RoomPID),
%                      NewState = #state{
%                        roomsByShortID=dict:store(ShortID, {RoomKey, RoomPID}, RoomByID),
%                        roomKeys=RoomKeysRemaining,
%                        userKeys=UserKeysRemaining},
%                      %%at long last, success.
%                      {reply, {ok, {RoomKey, UID, ShortID}, {UserKey, UserID}}, NewState}
%                  end
%              end
%          end
%      end
%  end.





%
%      case registrationServer:getNextKey() of
%        {ok, Key} ->
%          case roomServer:start_link(Key, UID, Name) of
%            {ok, Pid} ->
%              case register(Key, Pid) of
%                true ->
%                  Record = {Key, Name, UID},
%                  State2 = dict:store(ShortID, Record, State),
%                  {reply, {ok, ShortID, UID, Key}, State2};
%                _ ->
%                  {reply, error, State}
%              end;
%            _ ->
%              {reply, error, State}
%          end;
%        noKey ->
%          {reply, error, State}
%      end
%  end;


