-module(mural).

-export([make_validator/0]).


make_validator() ->
  {ok, NameRegex} = re:compile("^[a-zA-Z0-9_]+$"),
  mural_validate:new(NameRegex).


