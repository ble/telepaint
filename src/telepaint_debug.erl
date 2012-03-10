-module(telepaint_debug).


-include("room.hrl").
-export([send_to/2, send_to/3]).

 as_list(Msg) when is_list(Msg) -> Msg
;as_list(Msg) -> [Msg]
.

send_to(RoomId, Msg) ->
  {ok, RoomPid} = nexus:lookup_room(nexus, RoomId)
 ,{ok, Observers} = room:get_observers(RoomPid)
 ,QueuePids = [Observer#player.pid || Observer <- Observers]
 ,Messages = [json_rpc:as_call(M) || M <- as_list(Msg)]
 ,[player_queue:enqueue(Pid, Messages) || Pid <- QueuePids]
 .

send_to(RoomId, ObserverId, Msg) ->
  {ok, RoomPid} = nexus:lookup_room(nexus, RoomId)
 ,{ok, Observer} = room:get_observer(RoomPid, ObserverId) 
 ,QueuePid = Observer#player.pid
 ,Messages = [json_rpc:as_call(M) || M <- as_list(Msg)]
 ,player_queue:enqueue(QueuePid, Messages)
 .

% telepaint_debug:send_to(
%  <<"iBROZkdKj9FGlp95WzVgde-m">>,
%  <<"8lHIh9bHDZC9ajB0SMCBbves">>,
%  {[{<<"method">>, <<"swizzle">>}]}).     

% telepaint_debug:send_to(
%  <<"iBROZkdKj9FGlp95WzVgde-m">>,
%  <<"8lHIh9bHDZC9ajB0SMCBbves">>,
%  {chat, <<...>>, <<"What's up, bro?">>}).     
