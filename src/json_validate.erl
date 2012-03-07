-module(json_validate).

-export([draw_part/1, draw_part_replace_time/2]).

draw_part_replace_time({PropList}, When) ->
  Keys = draw_part_keys(),
  Values = get_many(Keys, PropList),
  PropList0 = lists:zip(Keys, Values),
  PropList1 = [{<<"startTime">>, to_millis(When)} | PropList0],
  json_rpc:jif_obj(PropList1).

draw_part({PropList}) when is_list(PropList) ->
  Keys = draw_part_keys(),
  [Tag, Coordinates, Times, Controls, LineWidth, StrokeStyle, FillStyle] =
   get_many(Keys, PropList),
  tag_valid(Tag)
   andalso color_valid(StrokeStyle)
   andalso color_valid(FillStyle)
   andalso points_valid(Times, Coordinates, Controls).

get_many([], _) ->
  [];
get_many([Key | Keys], PropList) ->
  [proplists:get_value(Key, PropList) |
    get_many(Keys, PropList)].

points_valid(Times, Coordinates, Controls) ->
  NTimes = length(Times),
  OnePointPerTime = (length(Coordinates) div 2 == NTimes),
  ControlIndicesValid = Controls == undefined orelse
                        lists:all(fun(Ix) -> Ix < NTimes end, Controls),
  TwoCoordinatesPerPoint = (length(Coordinates) rem 2 == 0),
  OnePointPerTime and ControlIndicesValid and TwoCoordinatesPerPoint.

color_valid(undefined) ->
  true;

color_valid(ColorString) ->
  RgbNumber = "(\\d{1,3})",
  AlphaNumber = "([01](?:\\.\\d+)?)",
  CommaMaybeSpaces = ",\\s*",
  Pattern = [
    "rgba\\(",
    RgbNumber,
    CommaMaybeSpaces,
    RgbNumber,
    CommaMaybeSpaces,
    RgbNumber,
    CommaMaybeSpaces,
    AlphaNumber,
    "\\)"],
  {ok, Compiled} = re:compile(Pattern),
  case re:run(ColorString, Compiled) of
    {match, [_, R, G, B, A]} ->
      [RStr, GStr, BStr, AStr] = [binary_part(ColorString, X) || X <- [R,G,B,A]],
      RGB = [string:to_integer(Str) || Str <- [RStr, GStr, BStr]],
      A = string:to_float(AStr),
      lists:all(fun(X) -> X >= 0 andalso X =< 255 end, RGB)
        andalso A >= 0
        andalso A =< 1.0;
    _ ->
      false
  end.

tag_valid(Tag) ->
  (Tag == <<"ble._2d.StrokeReplay">>
    orelse Tag == <<"ble._2d.PolylineReplay">>
    orelse Tag == <<"ble._2d.EraseReplay">>).

draw_part_keys() ->
  [ <<"_tag">>,
    <<"coordinates">>,
    <<"times">>,
    <<"controls">>,
    <<"lineWidth">>,
    <<"strokeStyle">>,
    <<"fillStyle">>].

-define(E6, 1000000).
-define(E3, 1000).
to_millis({X, Y, Z}) ->
  ((X * ?E6) + Y) * ?E3 + (Z div ?E3).
