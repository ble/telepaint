-module(mural_transaction).

-export([hash/0]).
-export([make_mural/2, get_user/2, get_mural/1, update_user_response/2]).

-include("records.hrl").
-include_lib("stdlib/include/qlc.hrl").

hash() ->
  {Mega, Unit, Micro} = erlang:now(),
  NowBin = <<Mega:20, Unit:20, Micro:20, ((Unit + Micro) rem 1024):12>>,
  {ok, BadChar} = re:compile("\\+|/"),
  re:replace(base64:encode(NowBin), BadChar, "_", [global, {return, list}]).


make_mural(MuralName, Peer) ->
  Timestamp = erlang:now(),
  MuralHash = hash(),
  CreatorHash = hash(),
  MuralRecord = #mural{
    mural_hash = MuralHash,
    timestamp = Timestamp,
    mural_name = MuralName},
  CreatorRecord = #user{
    user_hash = CreatorHash,
    bound_host = Peer,
    mural_hash = MuralHash,
    user_type = creator,
    last_msg_time = Timestamp},
  io:format("~p~n~p~n", [MuralRecord, CreatorRecord]),
  Transaction = fun() ->
      mnesia:write(MuralRecord),
      mnesia:write(CreatorRecord),
      ok
  end,
  case mnesia:transaction(Transaction) of
    {atomic, ok} -> {ok, MuralHash, CreatorHash};
    {aborted, Reason} -> {error, Reason}
  end.

get_user(MuralHash, UserHash) ->
  T = fun() ->
    C = qlc:q([X ||
      X <- mnesia:table(user),
      X#user.mural_hash =:= MuralHash,
      X#user.user_hash =:= UserHash]),
    qlc:e(C)
  end,
  mnesia:transaction(T). 

get_mural(MuralHash) ->
  T = fun() ->
    C = qlc:q([X ||
      X <- mnesia:table(mural),
      X#mural.mural_hash =:= MuralHash]),
    qlc:e(C)
  end,
  mnesia:transaction(T).

update_user_response(UserHash, Response) ->
  T = fun() ->
    case mnesia:read({user, UserHash}) of
      [User] ->
        mnesia:write(User#user{resp_current = Response});
      _ ->
        mnesia:abort(no_such_user)
    end
  end,
  mnesia:transaction(T).
