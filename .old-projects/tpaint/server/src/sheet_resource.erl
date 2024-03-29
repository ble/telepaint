%% @author author <author@example.com>
%% @copyright YYYY author.
%% @doc Example webmachine_resource.

-module(sheet_resource).
-export([init/1, content_types_provided/2, allowed_methods/2, forbidden/2]).
-export([encodings_provided/2]).
-export([content_types_accepted/2, from_json/2]).
-export([resource_exists/2, previously_existed/2, moved_permanently/2]).
-export([to_json/2, process_post/2]).

-include_lib("webmachine/include/webmachine.hrl").
-include("sheet.hrl").
-include("tpaint_util.hrl").

init([]) -> {ok, stateless}.

content_types_provided(Req, State) ->
  {[{"application/json", to_json}, {"text/json", to_json}], Req, State}.

content_types_accepted(Req, State) ->
  {[{"application/json", from_json}], Req, State}.

from_json(Req, State) ->
  debug_msg("content type recognized~n", []),
  {process_post(Req, State), Req, State}.

encodings_provided(Req, State) ->
  Encs = case wrq:method(Req) of
    'GET' ->
      [{"identity", fun(X) -> X end},
       {"gzip", fun(X) -> zlib:gzip(X) end}];
    _ ->
      [{"identity", fun(X) -> X end}]
  end,
  {Encs, Req, State}.

allowed_methods(Req, stateless) ->
  {['GET', 'HEAD', 'POST'], Req, stateless}.

forbidden(Req, stateless) ->
  Sheets = get_sheet(Req),
  debug_msg("Requested sheet: ~p~n", [Sheets]),
  case Sheets of
    bad_path ->
      {false, Req, {sheet, undefined}};
    [] ->
      {false, Req, {sheet, undefined}}; 
    [Sheet] ->
      SessionId = get_session_id(Req), 
      debug_msg("sheet access: ~p~n", [Sheet#sheet.session_access]),
      Access = case Sheet#sheet.session_access of
        AccessList when is_list(AccessList) ->
          proplists:get_value(SessionId, AccessList);
        AccessType ->
          AccessType
      end,
      debug_msg("access: ~p~nmethod: ~p~n", [Access, wrq:method(Req)]),
      Forbidden = case {Access, wrq:method(Req)} of
        {write, _} -> false;
        {read, 'POST'} -> true;
        {read, _} -> false;
        _ -> true
      end,
      {Forbidden, Req, {sheet, Sheet}}
  end.



resource_exists(Req, {sheet, undefined}) ->
  {false, Req, {sheet, undefined}}; 
resource_exists(Req, State = {sheet, #sheet{complete_url = undefined}}) ->
  {true, Req, State};
resource_exists(Req, State) ->
  {false, Req, State}.

previously_existed(Req, {sheet, undefined}) ->
  {false, Req, undefined};
previously_existed(Req, State) ->
  {true, Req, State}.

moved_permanently(Req, State = {sheet, #sheet{complete_url = Url}}) ->
  {{true, Url}, Req, State}.


to_json(Req, {sheet, Sheet = #sheet{id = SheetId}}) ->
  FragT = fun() -> mnesia:index_read(sheet_fragment, SheetId, #sheet_fragment.id) end,
  debug_msg("#sheet_fragment.id -> ~p~n", [#sheet_fragment.id]),
  T = mnesia:transaction(FragT),
  case T of
    {atomic, JsonFragments} ->
      {assemble_json(Sheet, JsonFragments), Req, Sheet};
    {error, Reason} ->
      {io_lib:format("'~p'", [Reason]), Req, Sheet}
  end.

process_post(Req, {sheet, #sheet{id = SheetId}}) ->
  Body0 = wrq:req_body(Req),
  Body1 = jiffy:decode(Body0),
  debug_msg("json: ~p~n", [Body1]),
  {ok, {Method0, Data, _}} = tpaint_rpc:plain(Body1),
  Method = case Method0 of
    <<"stroke">> -> stroke;
    <<"undo">> -> undo;
    %<<"clear">> -> clear;
    <<"erase">> -> erase 
  end,
  debug_msg("{method, data}: ~p~n", [{Method, Data}]),
  Stamp = now(),
  InsertT = fun() -> mnesia:write(#sheet_fragment{timestamp=now(), id=SheetId, json=Body1}) end,
      case mnesia:transaction(InsertT) of
    {atomic, ok} ->
      SuccessBody = jiffy:encode({[{status, ok}, {stamp, tuple_to_list(Stamp)}]}),
      {true, wrq:set_resp_body(SuccessBody, Req), stateless};
    _ ->
      EBody = jiffy:encode({[{status, error}]}),
      {false, wrq:set_resp_body(EBody, Req), stateless}
  end.

  
get_sheet(Req) ->
  Dp = wrq:disp_path(Req),
  Pir = wrq:path_info(sheet_id, Req),
  debug_msg("disp_path -> ~p~npath_info(sheet_id) -> ~p~n", [Dp, Pir]),
  case wrq:disp_path(Req) of
    [] ->
      case wrq:path_info(sheet_id, Req) of
        undefined ->
          no_sheet;
        SheetId0 ->
          SheetId = erlang:list_to_binary(SheetId0),
          debug_msg("Requested sheet id: ~p~n", [SheetId]),
          case mnesia:transaction(fun() -> mnesia:read({sheet, SheetId}) end) of
            {atomic, Result} ->
              Result;
            _ ->
              error
          end
      end;
    _ ->
      bad_path
  end.

get_session_id(Req) ->
  wrq:get_cookie_value("session", Req).

assemble_json(Sheet, Fragments) ->
  debug_msg("foo~n", []),
  FragmentJson = [F#sheet_fragment.json || F <- Fragments],
  Eson = {[{<<"id">>, Sheet#sheet.id}, {<<"fragments">>, FragmentJson}]},
  debug_msg("eson output: ~p~n", [Eson]),
  jiffy:encode(Eson).
