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
  {StackDirectory, ArtistDirectory} = directoriesForPicRef(StoreRoot, PicRef),
  io:format(StackDirectory, []),
  io:format(ArtistDirectory, []),
  case {ensureDirectoryExists(StackDirectory), ensureDirectoryExists(ArtistDirectory)} of
    {ok, ok} ->
      io:format("kate moss confidence", []),
      {ok, Extension} = getExtension(PicData),
      io:format("kate moss confidence", []),
      {ok, Binary} = decodePicture(PicData),
      io:format("kate moss confidence", []),
      {SPath, APath} = filesForPicRef(StoreRoot, PicRef, Extension),
      io:format("kate moss confidence", []),

      io:format("~p~n", [SPath]),
      case file:write_file(SPath, Binary) of
        {error, Reason} ->
          {reply, {error, Reason}, State};
        ok -> 
          case file:make_link(SPath, APath) of
            {error, Reason} ->
              {reply, {partial, Reason}, State};
            ok ->
              {reply, {ok, SPath}, State}
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

directoriesForPicRef(
  StoreRoot,
  #picRef{
    roomName = RoomName,
    roomID = RoomID,
    startDate = StartDate,
    startedBy = Starter,
    startedID = SID,
    drawnBy = Artiste,
    drawnID = DID,
    n = N}) ->
  RoomDirectory = [StoreRoot, $/, RoomName, RoomID],
  GameDirectory = [RoomDirectory, $/, formatDate(StartDate)],
  SDirectory = [GameDirectory, $/, startedBy, $=, Starter, SID],
  ADirectory = [GameDirectory, $/, artist, $=, Artiste, DID],
  {fileNameAsString(SDirectory), fileNameAsString(ADirectory)}.

filesForPicRef(
  StoreRoot,
  PicRef = #picRef{
    n = N,
    drawnBy = DrawnBy,
    startedBy = StartedBy},
  Extension) ->
  {StackDirectory, ArtistDirectory} = directoriesForPicRef(StoreRoot, PicRef), 
  NStr = io_lib:fwrite("~3..0B", [N]),
  SPath = [StackDirectory, $/, NStr, DrawnBy, Extension],
  APath = [ArtistDirectory, $/, NStr, StartedBy, Extension],
  {SPath, APath}.

fileNameAsString(Name) ->
  case lists:all(fun erlang:is_integer/1, Name) of
    true ->
      Name;
    false when is_atom(Name) ->
      erlang:atom_to_list(Name);
    false when is_list(Name) ->
      fileNameAsString([], Name);
    false when is_binary(Name) ->
      {error, "don't know how to handle the raw filename case."}
  end.

fileNameAsString(Accum, []) ->
  lists:flatten(lists:reverse(Accum));

fileNameAsString(Accum, [Char | Rest]) when is_integer(Char) ->
  {AlsoInString, Remainder} = lists:splitwith(fun erlang:is_integer/1, Rest),
  Prefix = [Char | AlsoInString],
  fileNameAsString([Prefix | Accum], Remainder);

fileNameAsString(Accum, [Atom | Rest]) when is_atom(Atom) ->
  fileNameAsString([erlang:atom_to_list(Atom) | Accum], Rest); 

fileNameAsString(Accum, [DeepList | Rest]) when is_list(DeepList) ->
  Deeper = fileNameAsString([], DeepList),
  fileNameAsString([Deeper | Accum], Rest).

formatDate({{Y, M, D}, {H, Min, S}}) ->
  lists:flatten(io_lib:format("~4..0B.~2..0B.~2..0B_~2..0B.~2..0B.~2..0B", [Y, M, D, H, Min, S])).
