-module(sheet_db).

-include("sheet.hrl").

-export([make_tables/1, make_sheet/2]).

make_sheet(Id, Access) ->
  fun() ->
      mnesia:write(#sheet{id=Id, session_access=Access})
  end.

make_tables(NodeList) ->
  TableDefinitions =
    [{sheet, [
       {attributes, record_info(fields, sheet)},
       {disc_copies, NodeList}]},
     {sheet_fragment, [
       {attributes, record_info(fields, sheet_fragment)},
       {disc_copies, NodeList},
       {index, [id]},
       {type, ordered_set}]}],
  [{Name, Result} ||
   {Name, TabDef} <- TableDefinitions,
   Result <- [mnesia:create_table(Name, TabDef)]].

