-module(mural).

-export([hash/0, make_validator/0]).
-export([make_mural/2, get_mural/3]).
-include("records.hrl").
-include_lib("stdlib/include/qlc.hrl").


make_validator() ->
  {ok, NameRegex} = re:compile("^[a-zA-Z0-9_]+$"),
  mural_validate:new(NameRegex).

hash() ->
  {Mega, Unit, Micro} = erlang:now(),
  NowBin = <<Mega:20, Unit:20, Micro:20, ((Unit + Micro) rem 1024):12>>,
  {ok, BadChar} = re:compile("\\+|/"),
  re:replace(base64:encode(NowBin), BadChar, "_", [global, {return, list}]).

make_mural(Req, MuralName) ->
  Timestamp = erlang:now(),
  Host = Req:get(peer),
  MuralHash = hash(),
  CreatorHash = hash(),
  MuralRecord = #mural{
    mural_hash = MuralHash,
    timestamp = Timestamp,
    mural_name = MuralName},
  CreatorRecord = #user{
    user_hash = CreatorHash,
    bound_host = Host,
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
    {atomic, ok} ->
      {ok, MuralHash, CreatorHash};
    {aborted, Reason} ->
      {error, Reason}
  end.

get_mural(Req, MuralHash, observer) ->
  GetTransaction = fun() ->
    Comprehension = qlc:q([
      X#mural.mural_name ||
      X <- mnesia:table(mural),
      X#mural.mural_hash =:= MuralHash]),
    qlc:e(Comprehension)
  end,
  mnesia:transaction(GetTransaction);

get_mural(Req, MuralHash, UserHash) ->
  GetTransaction = fun() ->
    Comprehension = qlc:q([
      {X#mural.mural_name, Y#user.user_type, Y#user.bound_host} ||
      X <- mnesia:table(mural),
      X#mural.mural_hash =:= MuralHash,
      Y <- mnesia:table(user),
      Y#user.mural_hash =:= MuralHash,
      Y#user.user_hash =:= UserHash]),
    qlc:e(Comprehension)
  end,
  mnesia:transaction(GetTransaction).

