%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the tpaint application.

-module(tpaint_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for tpaint.
start(_Type, _StartArgs) ->
    tpaint_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for tpaint.
stop(_State) ->
    ok.
