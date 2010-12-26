-module(tpaint_util).
-export([redirectTo/3, textResponse/2, cookies/1, respondJSON/2,
         getRoomForReq/2,
         shorten/1,
         getRoomUID/2, getPicUID/3, getUserUID/2,
         threeListToTriplet/1, tripletToThreeList/1,
         sfmt/2, pr/1]).


sfmt(Format, Items) ->
  lists:flatten(io_lib:fwrite(Format, Items)).
pr(String) ->
  io:format("~p~n", [String]).

shorten(String) ->
  if
    length(String) >= 8 ->
      string:sub_string(String, 1, 8);
    true ->
      String
  end.


cookies(Items) ->
  lists:map(fun (Item) ->
    case Item of
      {Key, Value} ->
        mochiweb_cookies:cookie(Key, Value);
      {Key, Value, Opts} ->
        mochiweb_cookies:cookie(Key, Value, Opts)
    end end,
  Items).
respondJSON(Req, Term) ->
  Req:ok({"application/json", mochijson:encode(Term)}).

getRoomForReq(ShortID, Req) ->
  case Req:get_cookie_value("key") of
    undefined ->
      {uncookied, nexus:getRoom(ShortID)};
    KeyStr ->
      Key = erlang:list_to_atom(KeyStr),
      case whereis(Key) of
        undefined ->
          {cookied, undefined};
        _ ->
          {cookied, {ok, Key}}
      end
  end.

tripletToThreeList({A, B, C}) ->
  [A, B, C].

threeListToTriplet([A, B, C]) ->
  {A, B, C}.

redirectTo(Req, Location, OtherHeaders) ->
  Req:respond(
    {302,
      [{"Location", Location} | OtherHeaders] ++ [{"Content-Type", "text/html; charset=UTF-8"}],
     ""}).  

textResponse(Req, Body) ->
  Req:ok({"text/plain", Body}).  

getPicUID(RoomName, CreatorIPString, PicSnippet) ->
  md5Nonce([RoomName, ip4To32Bits(CreatorIPString), PicSnippet]).

getRoomUID(RoomName, UserIPString) ->
  md5Nonce([RoomName, ip4To32Bits(UserIPString)]).

getUserUID(RoomUID, UserIPString) ->
  md5Nonce([RoomUID, ip4To32Bits(UserIPString)]).


md5Collect(List) ->
  md5Collect(List, erlang:md5_init()).

md5Collect([Item | Rest], Context) ->
  md5Collect(Rest, erlang:md5_update(Context, Item));

md5Collect([], Context) ->
  Digest = erlang:md5_final(Context),
  UID = lists:foldl(fun(E, A) -> 256 * A + E end, 0, erlang:binary_to_list(Digest)),
  string:to_lower(erlang:integer_to_list(UID, 36)).


md5Nonce(List) ->
  md5Collect([getTime_micros64Bits() | List]).

getTime_micros64Bits() ->
  {Megaseconds, Seconds, Microseconds} = erlang:now(),
  AllMicros = ((Megaseconds * 1000000) + Seconds) * 1000000 + Microseconds,
  <<AllMicros:64>>.

split(String, [Delimiter]) ->
  NWords = string:words(String, Delimiter),
  lists:map(
    fun(N) -> string:sub_word(String, N, Delimiter) end,
    lists:seq(1, NWords)).

ip4To32Bits(IpString) ->
  AsIntegers = lists:map(fun erlang:list_to_integer/1, split(IpString, ".")),
  erlang:list_to_binary(AsIntegers).


