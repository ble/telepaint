-module(mural_db).

-export([create_db_tables/1]).
-include("records.hrl").

create_db_tables(Nodes) ->
  Tables = [
    {mural, [
      {disc_copies, Nodes},
      {attributes, record_info(fields, mural)}
      ]},
    {user, [
      {attributes, record_info(fields, user)},
      {disc_copies, Nodes}]},
    {current_img, [
      {attributes, record_info(fields, img)},
      {disc_copies, Nodes}]},
    {complete_img, [
      {attributes, record_info(fields, img)},
      {disc_only_copies, Nodes}]},
    {chat, [
      {attributes, record_info(fields, chat)},
      {disc_copies, Nodes}]},
    {current_chat, [
      {attributes, record_info(fields, condense_log)},
      {disc_copies, Nodes}]},
    {complete_chat, [
      {attributes, record_info(fields, condense_log)},
      {disc_only_copies, Nodes}]},
    {stroke, [
      {attributes, record_info(fields, stroke)},
      {disc_copies, Nodes}]},
    {current_stroke, [
      {attributes, record_info(fields, condense_log)},
      {disc_copies, Nodes}]},
    {complete_stroke, [
      {attributes, record_info(fields, condense_log)},
      {disc_only_copies, Nodes}]}],
  Results = [
    {Name, Result} ||
      {Name, TabDef} <- Tables,
      _ <- [io:format("~p~n", [{Name, TabDef}])],
      Result <- [mnesia:create_table(Name, TabDef)]],
  Successes = [
    Name ||
      {Name, {atomic, ok}} <- Results],
  Failures = [
    {Name, Reason} ||
      {Name, {aborted, Reason}} <- Results],
  [{success, Successes}, {failure, Failures}].


