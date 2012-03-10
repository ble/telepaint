%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc tpaint startup code

-module(tpaint).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0, db_init/1]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

db_init(sheet) ->
  sheet_db:make_tables([node()]).

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(crypto),
    ensure_started(inets),
    ensure_started(mochiweb),
    ensure_started(mnesia),
    db_init(sheet),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    tpaint_sup:start_link().

%% @spec start() -> ok
%% @doc Start the tpaint server.
start() ->
    ensure_started(crypto),
    ensure_started(inets),
    ensure_started(mochiweb),
    ensure_started(mnesia),
    db_init(sheet),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(tpaint).

%% @spec stop() -> ok
%% @doc Stop the tpaint server.
stop() ->
    Res = application:stop(tpaint),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    Res.
