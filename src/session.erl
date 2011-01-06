-module(session).
-export([makeCreatorSession/2, makeUserSession/2, getSession/1]).

makeCreatorSession(RoomName, CreatorIPString) ->
  RoomID = getRoomUID(RoomName, CreatorIPString),
  CreatorID = getUserUID(RoomID, CreatorIPString),
  Cookies = cookies([{"roomID", RoomID},{"userID", CreatorID}]),
  {RoomID, CreatorID, Cookies}.

makeUserSession(RoomID, UserIPString) ->
  UserID = getUserUID(RoomID, UserIPString),
  Cookies = cookies([{"roomID", RoomID},{"userID", UserID}]),
  {RoomID, UserID, Cookies}.

getSession(Req) ->
  case {Req:get_cookie_value("roomID"), Req:get_cookie_value("userID")} of
    {undefined, _} -> undefined;
    {_, undefined} -> undefined;
    {undefined, undefined} -> undefined;
    Found -> Found
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


