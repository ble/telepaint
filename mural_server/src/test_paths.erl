
-module(test_paths).
-export([run_test/0, test_paths/1]).

run_test() ->
  MuralName = "HorseComWebDotBiz",
  {{get, GetPaths}, {post, PostPaths}} = test_paths(MuralName),
  GetUrls = [base() ++ GetPath || GetPath <- GetPaths],
  PostUrls = [base() ++ PostPath || PostPath <- PostPaths],
  Gets = [httpc:request(GetUrl) ||
    GetUrl <- GetUrls],
  Posts = [httpc:request(post, {PostUrl, []}, [], []) ||
    PostUrl <- PostUrls],
%    Result <- 
%  [_Foo ||
%    Item <- Gets ++ Posts,
%    _Foo <- io:format("~p~n", Item)],
  ok.
%  GetResults =
%    [httpc:request(Url) ||
%      Path <- GetPaths,
%      Url <- base() ++ Path,
%      _ <- io:format("GET: ~p~n", [Url])],
%  PostResults =
%    [httpc:request(post, {Url, []}, [], []) ||
%      Path <- PostPaths,
%      Url <- base() ++ Path,
%      _ <- io:format("POST: ~p~n", [Url])],
%  Results =
%    [Result ||
%      {ok, Result} <- GetResults ++ PostResults],
%  Results.

base() ->
  "http://127.0.0.1:8080".

test_paths(MuralName) ->
  MuralHash = "f4k3h45h",
  CreatorHash = "cr34t0rh45h",
  ParticipantHash = "p4rt1c1p4nth45sh",
  GetURLs = 
  [Prefix ++ "/" ++ MuralName ++ Suffix ||
   {Prefix, Suffix} <- [
     {"/make_mural", ""},
     {"", MuralHash ++ "/" ++ CreatorHash},
     {"", MuralHash ++ "/" ++ CreatorHash ++ "/connect"},
     {"", MuralHash ++ "/" ++ CreatorHash ++ "/reconnect"},
     {"", MuralHash ++ "/" ++ ParticipantHash},
     {"", MuralHash ++ "/" ++ ParticipantHash ++ "/connect"},
     {"", MuralHash ++ "/" ++ ParticipantHash ++ "/reconnect"},
     {"", ""},
     {"", "/connect"},
     {"", "/reconnect"} 
   ]],
  PostURLs =
  [Prefix ++ "/" ++ MuralName ++ Suffix ||
    {Prefix, Suffix} <- [
     {"", MuralHash ++ "/" ++ CreatorHash ++ "/choose_image"},
     {"", MuralHash ++ "/" ++ CreatorHash ++ "/tile_size"},
     {"", MuralHash ++ "/" ++ CreatorHash ++ "/chat"},
     {"", MuralHash ++ "/" ++ CreatorHash ++ "/mural_done"},
     {"", MuralHash ++ "/" ++ ParticipantHash ++ "/name"},
     {"", MuralHash ++ "/" ++ ParticipantHash ++ "/chat"},
     {"", MuralHash ++ "/" ++ ParticipantHash ++ "/stroke"}
   ]],
  {{get, GetURLs}, {post, PostURLs}}.
