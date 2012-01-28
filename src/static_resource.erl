%% derived from demo fs resource
%% @author Bryan Fink <bryan@basho.com>
%% @author Andy Gross <andy@basho.com>
%% @author Justin Sheehy <justin@basho.com>
%% @copyright 2008-2009 Basho Technologies, Inc.

-module(static_resource).
-export([init/1]).
-export([allowed_methods/2,
         resource_exists/2,
         last_modified/2,
         content_types_provided/2,
         provide_content/2,
         generate_etag/2]).

 -export([file_contents/1]).

-define(FILE_ROOT, "priv/www/").
-record(context, {root,path=undefined,response_body=undefined,metadata=[]}).
-include_lib("kernel/include/file.hrl").
-include_lib("webmachine/include/webmachine.hrl").

file_contents(Name) ->
  AbsRoot = filename:absname(?FILE_ROOT),
  AbsPath = normalize_filename(filename:absname(Name, AbsRoot)),
  case is_filename_prefix(AbsRoot, AbsPath) of
    true ->
      file:read_file(AbsPath) 
    false ->
      {error, bad_path}
  end

init([]) ->
  {ok, #context{root=?FILE_ROOT}};
init([Path]) ->
  {ok, #context{root=?FILE_ROOT, path=Path}}.
    
allowed_methods(ReqData, Context) ->
  {['HEAD', 'GET'], ReqData, Context}.

strip(Name) ->
  case Name of
    ["/" | Rest] -> Rest;
    _ -> Name
  end.


file_path(#context{path=Path}, _) when Path =/= undefined ->
  strip(Path);
file_path(_, Name) ->
  strip(Name).

as_str(F) when is_binary(F) ->
  binary_to_list(F);
as_str(F) when is_list(F) ->
  F.

normalize_filename(F) when is_binary(F) ->
  normalize_filename(as_str(F));

normalize_filename(F) ->
  Parts = filename:split(F),
  filename:join(normalized(Parts)).

normalized([_, ".." | Rest]) ->
  normalized(Rest);
normalized(["." | Rest]) ->
  normalized(Rest);
normalized([X | Rest]) ->
  [X | normalized(Rest)];
normalized([]) ->
  [].

is_filename_prefix(Prefix, Path) ->
  lists:prefix(as_str(Prefix), as_str(Path)).

file_exists(Context, Name) ->
  AbsRoot = filename:absname(Context#context.root),
  RelativePath = file_path(Context, Name),
  AbsPath = normalize_filename(filename:absname(RelativePath, AbsRoot)),

%  io:format("1.~p~n2.~p~n3.~p~n", [AbsRoot, RelativePath, AbsPath]),
  case is_filename_prefix(AbsRoot, AbsPath) of
    false ->
%      io:format("Someone is a naughty monkey.~n", []),
      false;
    true ->
      case filelib:is_regular(AbsPath) of 
        true ->
          {true, AbsPath};
        false ->
          false
      end
  end.

resource_exists(ReqData, Context) ->
  Path = wrq:disp_path(ReqData),
  case file_exists(Context, Path) of 
    {true, _} ->
      %debug_msg("name: ~p~n", [Name]),
      {true, ReqData, Context};
    _ ->
      {false, ReqData, Context}
  end.

maybe_fetch_object(Context, Path) ->
    % if returns {true, NewContext} then NewContext has response_body
    case Context#context.response_body of
        undefined ->
            case file_exists(Context, Path) of 
                {true, FullPath} ->
                    {ok, Value} = file:read_file(FullPath),
                    {true, Context#context{response_body=Value}};
                false ->
                    {false, Context}
            end;
        _Body ->
            {true, Context}
    end.

content_types_provided(ReqData, Context) ->
    Path = case Context#context.path of
      undefined -> wrq:path(ReqData);
      X -> X
    end,
    CT = webmachine_util:guess_mime(Path),
    %debug_msg("~p~n", [{wrq:path(ReqData), Context#context.path, CT}]),
    {[{CT, provide_content}], ReqData,
     Context#context{metadata=[{'content-type', CT}|Context#context.metadata]}}.

provide_content(ReqData, Context) ->
    case maybe_fetch_object(Context, wrq:disp_path(ReqData)) of 
        {true, NewContext} ->
            Body = NewContext#context.response_body,
            {Body, ReqData, Context};
        {false, NewContext} ->
            {error, ReqData, NewContext}
    end.

last_modified(ReqData, Context) ->
    {true, FullPath} = file_exists(Context,
                                   wrq:disp_path(ReqData)),
    LMod = filelib:last_modified(FullPath),
    {LMod, ReqData, Context#context{metadata=[{'last-modified',
                    httpd_util:rfc1123_date(LMod)}|Context#context.metadata]}}.

hash_body(Body) -> mochihex:to_hex(binary_to_list(crypto:sha(Body))).

generate_etag(ReqData, Context) ->
    case maybe_fetch_object(Context, wrq:disp_path(ReqData)) of
        {true, BodyContext} ->
            ETag = hash_body(BodyContext#context.response_body),
            {ETag, ReqData,
             BodyContext#context{metadata=[{etag,ETag}|
                                           BodyContext#context.metadata]}};
        _ ->
            {undefined, ReqData, Context}
    end.
