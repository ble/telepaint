-module(test_comet_resource).

-export([init/1, to_json/2, content_types_provided/2]).

-define(STREAM, test_comet_stream).
-define(COMET_TIMEOUT, (30 * 1000)).

-include_lib("webmachine/include/webmachine.hrl").

%%%repl example:
%{ok, Pid} = event_stream:start_container().
%register(test_comet_stream, Pid).
%event_stream:add_stream(test_comet_stream, <<"streamid">>, []). 
%event_stream:add_stream_handler(test_comet_stream, <<"streamid">>, fun (Id, X) -> {ok, [X]} end).
% %--request 'http://127.0.0.1:8000/comet-test/bifflesnort/0/0/0' now--
% %or better yet, request 'http://127.0.0.1:8000/comet-test-debug', fire up
% %the JS console and type the following:
% %  var loop = new ble.net.MACometLoop("http://127.0.0.1:8000/comet-test/streamid", 45000, [0,0,0]);
% %  var types = [goog.net.EventType.COMPLETE, goog.net.EventType.ERROR];
% %  goog.events.listen(loop, types, function(e) {console.log(e);});
% %  loop.start();
% % and then
%event_stream:send_event(test_comet_stream, insert_eson_here).


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



