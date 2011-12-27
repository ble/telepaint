-module(id_unique).

-export([for/1]).

-define(SALT, list_to_integer("1e9b6810062b30ba113c125ca165079b", 16)).

for(_Kind) ->
  Salt = <<(?SALT):128>>,
  Now = now_bin(),
  Digest = md5_fold([Salt, Now]),
  Pad = [Digest, binary:part(Now, {byte_size(Now), -2})],
  Padded = iolist_to_binary(Pad),
  Based0 = base64:encode(Padded),
  {ok, PatPlus} = re:compile(<<"\\+">>),
  {ok, PatSlash} = re:compile(<<"/">>),
  Based1 = re:replace(Based0, PatPlus, <<"_">>, [global]),
  Based2 = re:replace(Based1, PatSlash, <<"-">>, [global]),
  iolist_to_binary(Based2).
  

md5_fold(L) ->
  md5_fold(L, erlang:md5_init()).

md5_fold([], Ctx) ->
  erlang:md5_final(Ctx);

md5_fold([H | T], Ctx) ->
  md5_fold(T, erlang:md5_update(Ctx, H)).

now_micros() ->
  MM = 1000000,
  {Mega, S, Micro} = erlang:now(),
  (Mega * MM + S) * MM + Micro.

now_bin() ->
  <<(now_micros()):64>>.
