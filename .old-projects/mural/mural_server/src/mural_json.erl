-module(mural_json).

-export([make_state/2]).
-include("records.hrl").

z(undefined) -> null;
z(X) -> X.

make_state(Mural, User) ->
  UserPart = case User#user.user_type of
    creator ->
      {[{type, creator}]};
    observer ->
      {[{type, observer}]};
    #participant{pseudonym = UserName, row = Row, col = Col} ->
      {[{type, participant},
        {name, z(UserName)},
        {row, z(Row)},
        {col, z(Col)}]}
  end,
  #mural{mural_name = Name,
         img_fetch_url = Url,
         img_local = LocalUrl,
         rows = Rows,
         columns = Cols} = Mural,
  MuralPart = {[
    {name, z(Name)},
    {img_url, z(Url)},
    {rows, z(Rows)},
    {cols, z(Cols)},
    {image_url, z(LocalUrl)}]},
  {[{mural, MuralPart}, {user, UserPart}]}.
