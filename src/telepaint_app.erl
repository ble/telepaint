%% @author Ben Ellis <benjaminster@gmail.com>
%% @copyright 2011 idem.

%% @doc Callbacks for the telepaint application.
%% Largely (entirely?) from the generated Webmachine application;
%% as things progress, this may change.

-module(telepaint_app).
-author("Ben Ellis <benjaminster@gmail.com>").
-define(APPNAME, telepaint).

-behaviour(application).
-export([start/2,stop/1]).
-export([warmup/0]).

warmup() ->
  application:load(?APPNAME),
  start_applications(get_application_start_dependencies(?APPNAME)),
  application:start(?APPNAME).

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

start_applications(AppNames) ->
  Results = [application:start(App) || App <- AppNames],
  io:format("~p~n", [Results]),
  [] = lists:filter(
    fun (Result) -> Result =/= ok end,
    Results),
  ok.


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for tpaint.
start(_Type, _StartArgs) ->
  nexus:start_link(1),
    telepaint_web_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for tpaint.
stop(_State) ->
    ok.
