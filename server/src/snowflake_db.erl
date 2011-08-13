-module(snowflake_db).

-include("snowflake.hrl").

-export([make_tables/1, make_snowflake/2, select_all/0]).

make_snowflake(Id, Access) ->
  fun() ->
      mnesia:write(#snowflake{id=Id, session_access=Access})
  end.

select_all() ->
  fun() ->
      mnesia:select(snowflake, [{'$1', [], ['$1']}])
  end.

make_tables(NodeList) ->
  TableDefinitions =
    [{snowflake, [
       {attributes, record_info(fields, snowflake)},
       {disc_copies, NodeList}]},
     {snowflake_fragment, [
       {attributes, record_info(fields, snowflake_fragment)},
       {disc_copies, NodeList},
       {index, [id]},
       {type, ordered_set}]}],
  [{Name, Result} ||
   {Name, TabDef} <- TableDefinitions,
   Result <- [mnesia:create_table(Name, TabDef)]].

