-module(mural_validate, [MuralNameRegex]).

-export([valid_mural_name/1]).


valid_mural_name(MuralName) ->
  case re:run(MuralName, MuralNameRegex) of
    {match, _} -> true;
    nomatch -> false
  end.

