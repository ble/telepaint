%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(static_resource).
-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").
-define(FILE_ROOT, "priv/www/").

init([]) -> {ok, undefined};
init(Path) -> {ok, Path}.

to_html(ReqData, undefined) ->
  Path = wrq:disp_path(ReqData),
  io:format("~p~n", [[?FILE_ROOT, Path]]),
  {ok, F} = file:read_file([?FILE_ROOT, Path]), 
  {F, ReqData, undefined};

to_html(ReqData, Path) ->
  io:format("~p~n", [[?FILE_ROOT, Path]]),
  {ok, F} = file:read_file([?FILE_ROOT, Path]), 
  {F, ReqData, Path}.
