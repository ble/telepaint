-module(tpaint_rpc).

-export([plain/1]).

plain(Eson) ->
  parse_plain(Eson).

parse_plain({PList}) when is_list(PList) ->
  Method = proplists:get_value(<<"method">>, PList),
  Data = proplists:get_value(<<"data">>, PList),
  case {Method, Data} of
    {undefined, _} -> {error, no_method};
    {_, undefined} -> {error, no_data};
    _ -> {ok, {Method, Data}}
  end;

parse_plain(_) ->
  {error, malformed}.
