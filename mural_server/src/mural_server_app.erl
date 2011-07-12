%% @author Ben Ellis <benjaminster@gmail.com>
%% @copyright Ben Ellis <benjaminster@gmail.com>

%% @doc Callbacks for the mural_server application.

-module(mural_server_app).
-author("Ben Ellis <benjaminster@gmail.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mural_server.
start(_Type, _StartArgs) ->
    mnesia:start(),
    mural_server_deps:ensure(),
    mural_server_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mural_server.
stop(_State) ->
    ok.
