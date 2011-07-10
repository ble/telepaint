%% @author Ben Ellis <benjaminster@gmail.com>
%% @copyright 2010 Ben Ellis <benjaminster@gmail.com>

%% @doc Web server for mural_server.

-module(mural_server_web).
-author("Ben Ellis <benjaminster@gmail.com>").

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    io:format("OPTIONS ARE ~p ~n", [Options1]),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),
    PathParts = string:tokens(Path, "/"),
    ServerQuip = {"Server", "Mural on Mochiweb(\"Such is Mango!\")"},
    HtmlHeader = {"content-type", "text/html"},
    TextHeader = {"content-type", "text/plain"},
    JsonHeader = {"content-type", "text/json"},
    RespondText = fun(Text) ->
      Req:respond({200, [ServerQuip, TextHeader], Text}) end,
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case PathParts of
                  ["test_chunked", Number] ->
                    BytesPerChunk = erlang:list_to_integer(Number),
                    Response = Req:respond({200, [ServerQuip, TextHeader], chunked}),
                    TheChunk = [64 + (X rem 26) || X <- lists:seq(0, BytesPerChunk-1)],
                    NumberOfChunks = 16384 div BytesPerChunk,
                    Delay = 5000 div NumberOfChunks, 
                    [ begin
                        Response:write_chunk(TheChunk),
                        timer:sleep(Delay)
                      end || _X <- lists:seq(1, NumberOfChunks)],
                    Response:write_chunk([]);

                  ["make_mural", MuralShortName] ->
                    RespondText(MuralShortName);
                  ["murals", MuralName] ->
                    RespondText(["observer, ", MuralName]);
                  ["murals", MuralName, "connect"] ->
                    RespondText(["observer, connect, ", MuralName]);
                  ["murals", MuralName, "reconnect"] ->
                    RespondText(["observer, reconnect, ", MuralName]);
                  ["murals", MuralName, UserId | Rest] ->
                    Prefix = [MuralName, ", ", UserId],
                    case Rest of
                      [] -> RespondText(Prefix);
                      ["connect"] -> RespondText([Prefix, ", connect"]);
                      ["reconnect"] -> RespondText([Prefix, ", reconnect"]);
                      _ -> Req:not_found([ServerQuip])
                    end;
                  _ ->
                      Req:serve_file(Path, DocRoot, [ServerQuip])
                end;
            'POST' ->
                case PathParts of
                  ["murals", MuralName, UserId, RpcMethod] ->
                    Prefix = [UserId, " on ", MuralName, " ", RpcMethod],
                    case RpcMethod of
                      "choose_image" -> RespondText(Prefix);
                      "tile_size" -> RespondText(Prefix);
                      "chat" -> RespondText(Prefix);
                      "mural_done" -> RespondText(Prefix);
                      "name" -> RespondText(Prefix);
                      "stroke" -> RespondText(Prefix);
                      _ -> Req:not_found([ServerQuip])
                    end;
                    _ ->
                        Req:not_found([ServerQuip])
                end;
            _ ->
                Req:respond({501, [ServerQuip], []})
        end

%        case Req:get(method) of
%            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
%                case PathParts of
%                    _ ->
%                        Req:serve_file(Path, DocRoot)
%                end;
%            'POST' ->
%                case Path of
%                    _ ->
%                        Req:not_found()
%                end;
%            _ ->
%                Req:respond({501, [], []})
%        end
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
            %% NOTE: mustache templates need \ because they are not awesome.
            Req:respond({500, [{"Content-Type", "text/plain"}],
                         "request failed, sorry\n"})
    end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

you_should_write_a_test() ->
    ?assertEqual(
       "No, but I will!",
       "Have you written any tests?"),
    ok.

-endif.
