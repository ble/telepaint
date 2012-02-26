-module(tests).

-compile([export_all]).

test0() ->
  {ok, NexusPid} = nexus:start_link(1),
  {ok, RoomId} = nexus:make_room(NexusPid, <<"Room Name Here">>),
  {ok, RoomPid} = nexus:lookup_room(NexusPid, RoomId),
  {ok, PlayerId} = room:add_observer(RoomPid),
  Result = room:name_observer(RoomPid, PlayerId, <<"hey you">>),
  {ok, Observers} = room:get_observers(RoomPid),
  {ok, Found} = room:has_observer(RoomPid, PlayerId),
  {
    NexusPid,
    RoomId,
    RoomPid,
    PlayerId,
    Result,
    Observers,
    Found
  }.

player_queue() ->
  Time0 = {0, 0, 0},
  {ok, Pid} = player_queue:start(),
  {Time1, X1} = player_queue:poll_after(Pid, Time0),
  {Time2, X2} = player_queue:poll_after(Pid, Time1),
  player_queue:enqueue(Pid, [asdf]),
  {Time3, X3} = player_queue:poll_after(Pid, Time2),
  {Time4, X4} = player_queue:poll_after(Pid, Time3),
  {Time5, X5} = player_queue:poll_after(Pid, Time0),
  {[Time1, Time2, Time3, Time4, Time5], {X1, X2, X3, X4, X5}}.
