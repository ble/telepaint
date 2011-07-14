%% @author Ben Ellis <benjaminster@gmail.com>
%% @copyright 2010 Ben Ellis <benjaminster@gmail.com>

%% @doc Web server for mural_server.

-module(mural_server_web).
-author("Ben Ellis <benjaminster@gmail.com>").

-export([start/1, stop/0, loop/2]).
-include("records.hrl").

%%%'External API

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
    Validator = mural:make_validator(),
    "/" ++ Path = Req:get(path),
    PathParts = string:tokens(Path, "/"),
    try
        case Req:get(method) of
%%%'GET and HEAD
            Method when Method =:= 'GET'; Method =:= 'HEAD' ->
                case PathParts of
                  ["make_mural", MuralName] ->
                    handle_make_mural(erlang:list_to_binary(MuralName), Req);

                  ["murals", MuralHash] ->
                    respond_text(Req, ["observer, ", MuralHash]);

                  ["murals", MuralHash, "connect", _Ignored] ->
                    respond_text(Req, ["observer, connect, ", MuralHash]);

                  %get rid of the reconnect URL?
                  ["murals", MuralHash, "reconnect"] ->
                    respond_text(Req, ["observer, reconnect, ", MuralHash]);

                  ["murals", MuralHash, UserHash | Rest] ->
                    handle_client(MuralHash, UserHash, Rest, Req); 
                  _ ->
                      Req:serve_file(Path, DocRoot, [server_quip()])
                end;
%%%.
%%%'POST
            'POST' ->
                case PathParts of
                  ["murals", MuralHash, UserId, RpcMethod] ->
                    handle_rpc(MuralHash, UserId, RpcMethod, Req);
                  _ ->
                    Req:not_found([server_quip()])
                end;
%%%.
              _ ->
                Req:respond({501, [server_quip()], []})
        end
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
%%%.
%% Internal API

%%%' Utility/constants
server_quip() -> {"Server", "Mural on Mochiweb(\"Such is Mango!\")"}.
html_header() -> {"content-type", "text/html"}.
text_header() -> {"content-type", "text/plain"}.
json_header() -> {"content-type", "text/json"}.

msg_header() ->
  {[{message_start, null}]}.

msg_footer() ->
  {[{message_end, null}]}.

chunked_message(Eson) ->
  jiffy:encode([msg_header(), Eson, msg_footer()]).

respond_text(Req, Text) ->
  Req:respond({200, [server_quip(), text_header()], Text}).

respond_error(Req, Text) ->
  Req:respond({500, [server_quip(), text_header()], [<<"Sorry! Code 500: ">>, Text]}).
%%%.
%%%' Handlers
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

handle_client0(User, Mural, Rest, Req) ->
  UserHash = User#user.user_hash,
  MuralHash = User#user.mural_hash,
  Resp0 = [MuralHash, " ", UserHash, io_lib:write(User#user.user_type)],
  case Rest of
    [] ->
      Req:serve_file("client.html", "priv/static", [server_quip()]);

    ["connect", _Ignored] ->
      Response = Req:respond({200, [server_quip(), json_header()], chunked}),
      Response:write_chunk(chunked_message(mural_json:make_state(Mural, User))),
      timer:sleep(1000),
      Response:write_chunk(chunked_message({[{hi,bye},{good,day}]})),
      timer:sleep(1000),
      OldResponse = User#user.resp_current,
      catch OldResponse:write_chunk([]),
      mural_transaction:update_user_response(UserHash, Response);

    ["reconnect"] ->
      respond_text(Req, [Resp0, ", reconnect"]);

    _ ->
      Req:not_found([server_quip()])
  end.

handle_client(MuralHash, UserHash, Rest, Req) ->
  case mural_transaction:get_client(MuralHash, UserHash) of
    {atomic, [{User, Mural}]} ->
      handle_client0(User, Mural, Rest, Req);

    {atomic, []} ->
      Req:not_found([server_quip()]);

    {aborted, Reason} ->
      respond_error(Req,io_lib:write(Reason))
  end.


handle_rpc0(User, Mural, "choose_image", Req) ->
  CometResponse = User#user.resp_current,
  CometResponse:write_chunk(chunked_message(<<"choose_image">>)),
  respond_text(Req, <<"ok">>);


handle_rpc0(User, Mural, RpcMethod, Req) ->
  UserId = User#user.user_hash,
  MuralHash = User#user.mural_hash,
  Prefix = [UserId, " on ", MuralHash, " ", RpcMethod],
  case RpcMethod of
    "tile_size" -> respond_text(Req, Prefix);
    "chat" -> respond_text(Req, Prefix);
    "mural_done" -> respond_text(Req, Prefix);
    "name" -> respond_text(Req, Prefix);
    "stroke" -> respond_text(Req, Prefix);
    _ -> Req:not_found([server_quip()])
  end.

handle_rpc(MuralHash, UserHash, RpcMethod, Req) -> 
  case mural_transaction:get_client(MuralHash, UserHash) of
    {atomic, [{User, Mural}]} ->
      handle_rpc0(User, Mural, RpcMethod, Req);
    _ ->
      Req:not_found([server_quip()])
  end.


%%%.
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
%%% vim: set filetype=erlang tabstop=2 foldmarker=%%%',%%%. foldmethod=marker:
