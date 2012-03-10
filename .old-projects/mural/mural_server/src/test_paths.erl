
-module(test_paths).
-export([run_test/1, test_paths/1]).

run_test(get) ->
  MuralName = "HorseComWebDotBiz",
  {{get, GetPaths}, {post, _PostPaths}} = test_paths(MuralName),
  GetUrls = [base() ++ GetPath || GetPath <- GetPaths],
  Gets = [httpc:request(GetUrl) ||
    GetUrl <- GetUrls],
  [io:format("~p~n", [Item]) ||
    Item <- Gets],
  ok;

run_test(post) ->
  MuralName = "HorseComWebDotBiz",
  {{get, _GetPaths}, {post, PostPaths}} = test_paths(MuralName),
  PostUrls = [base() ++ PostPath || PostPath <- PostPaths],
  Posts = [
    begin
      timer:sleep(50),
      httpc:request(post, {PostUrl, [], "text/json", "'hi'"}, [], [])
    end
      ||
    PostUrl <- PostUrls],
  [io:format("~p~n", [Item]) ||
    Item <- Posts],
  ok.

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
     {"/murals", MuralHash ++ "/" ++ CreatorHash},
     {"/murals", MuralHash ++ "/" ++ CreatorHash ++ "/connect"},
     {"/murals", MuralHash ++ "/" ++ CreatorHash ++ "/reconnect"},
     {"/murals", MuralHash ++ "/" ++ ParticipantHash},
     {"/murals", MuralHash ++ "/" ++ ParticipantHash ++ "/connect"},
     {"/murals", MuralHash ++ "/" ++ ParticipantHash ++ "/reconnect"},
     {"/murals", ""},
     {"/murals", "/connect"},
     {"/murals", "/reconnect"} 
   ]],
  PostURLs =
  [Prefix ++ "/" ++ MuralName ++ Suffix ||
    {Prefix, Suffix} <- [
     {"/murals", MuralHash ++ "/" ++ CreatorHash ++ "/choose_image"},
     {"/murals", MuralHash ++ "/" ++ CreatorHash ++ "/tile_size"},
     {"/murals", MuralHash ++ "/" ++ CreatorHash ++ "/chat"},
     {"/murals", MuralHash ++ "/" ++ CreatorHash ++ "/mural_done"},
     {"/murals", MuralHash ++ "/" ++ ParticipantHash ++ "/name"},
     {"/murals", MuralHash ++ "/" ++ ParticipantHash ++ "/chat"},
     {"/murals", MuralHash ++ "/" ++ ParticipantHash ++ "/stroke"}
   ]],
  {{get, GetURLs}, {post, PostURLs}}.
