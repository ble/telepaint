%% @author Mochi Media <dev@mochimedia.com>
%% @copyright mural_server Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the mural_server application.

-module(mural_server_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for mural_server.
start(_Type, _StartArgs) ->
    mural_server_deps:ensure(),
    mural_server_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mural_server.
stop(_State) ->
    ok.
