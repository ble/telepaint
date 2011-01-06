-module(tpaint_util).
-export([redirectTo/3, textResponse/2, cookies/1, respondJSON/2,
         jsonError/1,
         getRoomForReq/2,
         shorten/1,
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

jsonError(Description) ->
  {struct, [{status, error}, {description, Description}]}.

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


