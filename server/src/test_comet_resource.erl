-module(test_comet_resource).

-export([ping/2, init/1, to_json/2, content_types_provided/2]).

-define(STREAM, test_comet_stream).
-define(COMET_TIMEOUT, (30 * 1000)).

%%%repl example:
%event_stream:add_stream(test_comet_stream, <<"bifflesnort">>, []). 
%event_stream:add_stream_handler(test_comet_stream, <<"bifflesnort">>, fun (Id, X) -> {ok, [X]} end).
% %--request 'http://127.0.0.1:8000/comet-test/bifflesnort/0/0/0' now--
%event_stream:send_event(test_comet_stream, <<"heyooooo">>).

init(Ctx) ->
  io:format("in init~n", []),
  Success = {ok, Ctx},
  case whereis(?STREAM) of
    undefined ->
      case event_stream:start_container() of
        {ok, Pid} ->
          register(?STREAM, Pid),
          Success;
        _ ->
          failed
      end;
    _ ->
      Success
  end.

ping(X, Y) ->
  {pong, X, Y}.


content_types_provided(Req, Ctx) ->
  io:format("in content_types_provided~n", []),
  {[{"application/json", to_json}], Req, Ctx}.

read_req_time(Req) ->
  [A, B, C] = [list_to_integer(wrq:path_info(X, Req)) || X <- [mega, unit, micro]],
  {A, B, C}.

now_to_array({A,B,C}) ->
  [A, B, C].

to_json(Req, Ctx) ->
  io:format("in to_json~n", []),
  Id = list_to_binary(wrq:path_info(id, Req)),
  When = read_req_time(Req),
  io:format("event_stream:get_stream_messages ~p~n", [{?STREAM, Id, When, ?COMET_TIMEOUT}]),
  case event_stream:get_stream_messages(?STREAM, Id, When, ?COMET_TIMEOUT) of
    timeout ->
      {jiffy:encode(no_data), Req, Ctx};
    {LastTime, Msgs} ->
      Stamp = {lastFetched, now_to_array(LastTime)},
      Data = {data, Msgs},
      {jiffy:encode({[Stamp, Data]}), Req, Ctx}
  end.



