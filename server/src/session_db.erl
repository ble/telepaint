-module(session_db).

-include("session.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([make_tables/1, make_session/2, select_recent/1, get/1]).

get(Id) ->
  fun() ->
      mnesia:read({session, Id})
  end.

make_tables(NodeList) ->
  Definitions =
    [{session,
      [{attributes, record_info(fields, session)},
       {disc_copies, NodeList}]}],
  [{Name, Result} ||
    {Name, TabDef} <- Definitions,
    Result <- [mnesia:create_table(Name, TabDef)]].

make_session(Name, Peer) ->
  fun() ->
    Instant = erlang:now(),
    MState0 = erlang:md5_init(),
    MState1 = erlang:md5_update(MState0, [Name, Peer, erlang:term_to_binary(Instant)]),
    Id = re:replace(base64:encode(erlang:md5_final(MState1)), <<"=">>, <<"_">>, [global, {return, binary}]),
    Record = #session{id=Id, name=Name, last_seen=Instant},
    case mnesia:write(Record) of
      ok -> Record;
      X -> X
    end 
  end.

select_recent(Seconds) ->
  Instant = now(),
  fun() ->
      Q = qlc:q([X || X <- mnesia:table(session),
                      timer:now_diff(Instant, X#session.last_seen) < Seconds * 1000000]),
      qlc:e(Q)
  end.
    
                      


