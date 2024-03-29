%% @author Ben Ellis <benjaminster@gmail.com>
%% @copyright 2011 idem.

%% @doc Helper for starting a single application by first identifying
%% its dependency applications and starting them.
%% Does not attempt to handle transitive dependencies, dependencies
%% listed out of order, etc.

-module(warmup).
-author("Ben Ellis <benjaminster@gmail.com>").

-export([start_app/1]).

start_app([AppName]) ->
  application:load(AppName),
  start_applications(get_application_start_dependencies(AppName), fun is_running/1),
  application:start(AppName);

start_app(AppName) ->
  start_app([AppName]).

get_application_start_dependencies(AppName) ->
  {ok, Dependencies} = application:get_key(AppName, applications),
  Running = [App || {App, _Desc, _Vsn} <- application:which_applications()], 
  DepsSet = sets:from_list(Dependencies),
  RunningSet = sets:from_list(Running),
  Needed = sets:subtract(DepsSet, RunningSet),
  ToStart = lists:filter(
    fun (App) -> sets:is_element(App, Needed) end,
    Dependencies),
  ToStart.

was_started(R) ->
  R =:= ok.

is_running(R) ->
  case R of
    ok -> true;
    {error, {already_started, _AppName}} -> true;
    true -> false
  end.

start_applications(AppNames, OkFilter) ->
  Results = [application:start(App) || App <- AppNames],
  io:format("~p~n", [Results]),
  [] = lists:filter(
    fun (Result) -> not OkFilter(Result) end,
    Results),
  ok.  
