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
