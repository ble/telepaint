%% @author Ben Ellis <benjaminster@gmail.com>
%% @copyright 2010 Ben Ellis <benjaminster@gmail.com>

%% @doc Web server for mural_server.

-module(mural_server_web).
-author("Ben Ellis <benjaminster@gmail.com>").

-export([start/1, stop/0, loop/2]).
-include("records.hrl").

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


server_quip() -> {"Server", "Mural on Mochiweb(\"Such is Mango!\")"}.
html_header() -> {"content-type", "text/html"}.
text_header() -> {"content-type", "text/plain"}.
json_header() -> {"content-type", "text/json"}.

msg_header() ->
  {[{message_start, null}]}.

msg_footer() ->
  {[{message_end, null}]}.

eson_to_iolist(Eson) ->
  jiffy:encode([msg_header(), Eson, msg_footer()]).

respond_eson(Req, Eson) ->
  Req:respond({200, [server_quip(), json_header()], jiffy:encode([msg_header(), Eson, msg_footer()])}).

respond_text(Req, Text) ->
  Req:respond({200, [server_quip(), text_header()], Text}).

respond_error(Req, Text) ->
  Req:respond({500, [server_quip(), text_header()], [<<"Sorry! Code 500: ">>, Text]}).

handle_make_mural(MuralName, Req) ->
  Validator = mural:make_validator(),
  case Validator:valid_mural_name(MuralName) of

    true ->
      case mural_transaction:make_mural(MuralName, Req:get(peer)) of

        {ok, MuralHash, CreatorHash} ->
          Redirect = {"Location", ["/murals/", MuralHash, "/", CreatorHash]},
          Cookie = mochiweb_cookies:cookie(
            "user_hash",
            CreatorHash,
            [{path, ["/murals/", MuralHash]}]),
          Headers = [server_quip(), Cookie, Redirect],
          Req:respond({302, Headers, ""});

        {error, Reason} ->
          respond_error(Req, io_lib:write(Reason))
      end;

    false ->
      respond_text(Req, "bad mural name")
  end.

loop(Req, DocRoot) ->
    Validator = mural:make_validator(),
    "/" ++ Path = Req:get(path),
    PathParts = string:tokens(Path, "/"),
    try
        case Req:get(method) of
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case PathParts of
                  ["make_mural", MuralName] ->
                    handle_make_mural(erlang:list_to_binary(MuralName), Req);

                  ["murals", MuralHash] ->
                    respond_text(Req, ["observer, ", MuralHash]);

                  ["murals", MuralHash, "connect"] ->
                    respond_text(Req, ["observer, connect, ", MuralHash]);

                  ["murals", MuralHash, "reconnect"] ->
                    respond_text(Req, ["observer, reconnect, ", MuralHash]);

                  ["murals", MuralHash, UserId | Rest] ->
                    case mural_transaction:get_user(MuralHash, UserId) of
                      {atomic, [User]} ->
                        Resp0 = [MuralHash, " ", UserId, io_lib:write(User#user.user_type)],
                        case Rest of
                          [] ->
                            Req:serve_file("client.html", "priv/static", [server_quip()]);
                          ["connect"] ->
                            case mural_transaction:get_mural(MuralHash) of
                              {atomic, [Mural]} ->
                                Response = Req:respond({200, [server_quip(), json_header()], chunked}),
                                Response:write_chunk(eson_to_iolist(mural_json:make_state(Mural, User))),
                                timer:sleep(1000),
                                Response:write_chunk(eson_to_iolist({[{hi,bye},{good,day}]})),
                                timer:sleep(1000),
                                Response:write_chunk([]);
                                
%                                respond_eson(Req, mural_json:make_state(Mural, User));
                              {atomic, []} ->
                                Req:not_found([server_quip()]);
                              {aborted, Reason} ->
                                respond_error(Req, io_lib:write(Reason))
                            end;
                          ["reconnect"] -> respond_text(Req, [Resp0, ", reconnect"]);
                          _ -> Req:not_found([server_quip()])
                        end;

                      {atomic, []} ->
                        Req:not_found([server_quip()]);

                      {aborted, Reason} ->
                        respond_error(Req,io_lib:write(Reason))
                    end;

                  _ ->
                      io:format("~p~n~p~n", [Path, DocRoot]),
                      Req:serve_file(Path, DocRoot, [server_quip()])
                end;
            'POST' ->
                case PathParts of
                  ["murals", MuralHash, UserId, RpcMethod] ->
                    Prefix = [UserId, " on ", MuralHash, " ", RpcMethod],
                    case RpcMethod of
                      "choose_image" -> respond_text(Req, Prefix);
                      "tile_size" -> respond_text(Req, Prefix);
                      "chat" -> respond_text(Req, Prefix);
                      "mural_done" -> respond_text(Req, Prefix);
                      "name" -> respond_text(Req, Prefix);
                      "stroke" -> respond_text(Req, Prefix);
                      _ -> Req:not_found([server_quip()])
                    end;
                    _ ->
                        Req:not_found([server_quip()])
                end;
            _ ->
                Req:respond({501, [server_quip()], []})
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
