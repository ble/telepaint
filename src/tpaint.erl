%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc tpaint.

-module(tpaint).
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
%% @doc Start the tpaint server.
start() ->
    tpaint_deps:ensure(),
    ensure_started(crypto),
    application:start(tpaint).


%% @spec stop() -> ok
%% @doc Stop the tpaint server.
stop() ->
    application:stop(tpaint).
