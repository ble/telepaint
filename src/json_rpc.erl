-module(json_rpc).
-export([unjif/1, jif/1]).
-export_records([rpc_call, rpc_response, rpc_response_error]).

-record(rpc_call,
  { version = <<"2.0">>,
    method = <<"unspecified_method">> :: binary(),
    params = [] :: [term()],
    id = undefined :: undefined | binary() | integer()
  }).

-record(rpc_response,
  { version = <<"2.0">>,
    result = undefined :: term(),
    error = undefined :: term(),
    id = undefined :: undefined | binary() | integer()
  }).

-record(rpc_response_error,
  { code = -1 :: integer(),
    message = <<"undescribed error">> :: binary(),
    data = none :: undefined | term()
  }).

jif_obj(X) ->
  {jif_obj_(X)}.

jif_obj_([])                   -> [];
jif_obj_([{_A, undefined} | X]) -> jif_obj_(X);
jif_obj_([{A, B} | X])         -> [{A, B} | jif_obj_(X)].

maybe_jif(X) ->
  try jif(X) catch error:_ -> X end.

jif(undefined) -> undefined;
jif(#rpc_call{version = V, method = M, params = P, id = I}) ->
  jif_obj([
    {<<"version">>, V},
    {<<"method">>, M},
    {<<"params">>, rpc_methods:unpopulate(P)},
    {<<"id">>, I}]);

jif(#rpc_response_error{code = C, message = M, data = D}) ->
  jif_obj([
    {<<"code">>, C},
    {<<"message">>, M},
    {<<"data">>, D}]);

jif(#rpc_response{version = V, result = R, error = undefined, id = I})
    when R =/= undefined ->
  jif_obj([
   {<<"version">>, V},
   {<<"result">>, rpc_methods:unpopulate(R)},
   {<<"id">>, I}]);


jif(#rpc_response{version = V, result = undefined, error = E, id = I})
    when E =/= undefined ->
  jif_obj([
    {<<"version">>, V},
    {<<"error">>, maybe_jif(E)},
    {<<"id">>, I}]).

unjif({PropList}) ->
  unjif_(PropList).

unjif_(PropList) ->
  Version = proplists:get_value(<<"version">>, PropList),
  Method = proplists:get_value(<<"method">>, PropList),
  Id = proplists:get_value(<<"id">>, PropList),
  Params = rpc_methods:populate(Method, proplists:get_value(<<"params">>, PropList)),
  #rpc_call{
    version = Version,
    method = Method,
    id = Id,
    params = Params
  }.

