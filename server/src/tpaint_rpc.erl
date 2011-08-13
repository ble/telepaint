-module(tpaint_rpc).

-export([plain/1]).

plain(Eson) ->
  parse_plain(Eson).

parse_plain({PList}) when is_list(PList) ->
  Method = proplists:get_value(<<"method">>, PList),
  Data = proplists:get_value(<<"data">>, PList),
  ClientTime = proplists:get_value(<<"startTime">>, Data),
  case {Method, Data, ClientTime} of
    {undefined, _, _} -> {error, no_method};
    {_, undefined, _} -> {error, no_data};
    {_, _, undefined} -> {error, no_time};
    X -> {ok, X}
  end;

parse_plain(_) ->
  {error, malformed}.
