-module(diskStore).
-include_lib("kernel/include/file.hrl").
-include_lib("state.hrl").
-behaviour(gen_server).


-export([start_link/1, shutdown/0, savePicture/2, makeDirectories/1]).

%%gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).  
-include_lib("debug_id.hrl").
start_link(StoreRoot) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {state, #stateDiskStore{storeRoot=StoreRoot}}, []).

savePicture(PicData, PicRef) ->
  gen_server:call(?MODULE, {savePicture, PicData, PicRef}).

shutdown() ->
  gen_server:call(?MODULE, shutdown).

init({state, State}) ->
  {ok, State}.

handle_call({savePicture, PicData, PicRef}, _, State = #stateDiskStore{storeRoot = StoreRoot}) ->
  {StackDirectory, ArtistDirectory} = state:directoriesForPicRef(StoreRoot, PicRef),
  case {ensureDirectoryExists(StackDirectory), ensureDirectoryExists(ArtistDirectory)} of
    {ok, ok} ->
      Extension = getExtension(PicData),
      Binary = decodePicture(PicData),
      {SPath, APath} = state:filesForPicRef(StoreRoot, PicRef, Extension),

      case file:write_file(SPath, Binary) of
        {error, Reason} ->
          {reply, {error, Reason}, State};
        ok -> 
          case file:make_link(SPath, APath) of
            {error, Reason} ->
              {reply, {partial, Reason}, State};
            ok ->
              {reply, ok, State}
          end
      end;
      _ ->
        {reply, {error, directories}, State}
  end;
 

% KEEP %  case ensureDirectoriesExist(
% KEEP %      StoreRoot,
% KEEP %      RName, RID, RStartDate,
% KEEP %      StartedBy, DrawnBy) of
% KEEP %    {error, Reason} ->
% KEEP %      {reply, {error, Reason}, State};
% KEEP %    {ok, StarterDir, DrawerDir} ->
% KEEP %      Extension = getExtension(PicData),
% KEEP %
% KEEP %      SPath = StarterDir ++ "/" ++ StarterName ++ UID ++ Extension, 
% KEEP %      DPath = DrawerDir ++ "/" ++ DrawerName ++ UID ++ Extension,
% KEEP %      Binary = decodePicture(PicData), 

handle_call(shutdown, _, State) ->
  {stop, normal, ok, State};

handle_call(Call, _, State) ->
  debug_id({unknownCall, Call}),
  {reply, {unknownCall, Call}, State}.

handle_cast(Cast, State) ->
  debug_id({unknownCast, Cast}),
  {noreply, State}.

handle_info(OOB, State) ->
  debug_id({unknownOOB, OOB}),
  {noreply, State}.

terminate(Reason, _) ->
  debug_id({stopping, Reason}).

code_change(_, State, _) ->
  {ok, State}.

ensureDirectoryExists(Path) ->
  case file:read_file_info(Path) of
    {ok, _} ->
      ok;
    _ -> 
      case makeDirectories(Path) of
        true -> ok;
        false -> error
      end
  end.

splitPath([$/ | PathString]) ->
  ["/" | splitPath(PathString)];

splitPath(PathString) ->
  {ok, Result} = regexp:split(PathString, "/"),
  Result.

makeDirectories(Path) ->
  makeDirectories_(splitPath(Path)).

makeDirectories_(PathParts) ->
  MustExist = [lists:sublist(PathParts, 1, N) || N <- lists:seq(1, length(PathParts))],
  Present = lists:map(
    fun (PathList) ->
        StrPath = string:join(PathList, "/"),
        case file:read_file_info(StrPath) of
          {ok, _} -> true;
          {error, _} ->
            case file:make_dir(StrPath) of
              ok -> true;
              _ -> false
            end
        end
    end,
    MustExist),
  lists:all(fun(X) -> X end, Present).


getExtension("data:image/png;base64," ++ _) ->
  {ok, ".png"};

getExtension(_) ->
  error.

decodePicture("data:image/png;base64," ++ Data) ->
  {ok, base64:decode(Data)};

decodePicture(_) ->
  error.


