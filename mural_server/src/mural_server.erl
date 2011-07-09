%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc mural_server.

-module(mural_server).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the mural_server server.
start() ->
    mural_server_deps:ensure(),
    ensure_started(crypto),
    application:start(mural_server).


%% @spec stop() -> ok
%% @doc Stop the mural_server server.
stop() ->
    application:stop(mural_server).
