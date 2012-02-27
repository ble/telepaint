-module(rpc_methods).

-compile({parse_transform, exprecs}).
-export([populate/2, unpopulate/1, unpopulate_response/1]).
-export_records([set_name, chat, join_room]).
-include("rpc_methods.hrl").

bin_to_atom(Binary) ->
  try
    erlang:binary_to_existing_atom(Binary, utf8)
  catch
    error:badarg -> undefined
  end.


set_bin({BinKey, Value}, Record) ->
  Atom = bin_to_atom(BinKey),
  case Atom of
    undefined -> Record;
    _ -> '#set-'([{Atom, Value}], Record)
  end.

populate(Method, {PropList}) ->
  Atom = bin_to_atom(Method),
  StartRecord = '#new-'(Atom),
  lists:foldl(
    fun set_bin/2, 
    StartRecord,
    PropList).

unpopulate(Record) 
    when is_tuple(Record)
    andalso is_atom(element(1, Record)) ->
  RecordName = element(1, Record),
  Fields = '#info-'(RecordName),
  { [{ atom_to_binary(Field, utf8),
       '#get-'(Field, Record) } || Field <- Fields]};

unpopulate(Json) ->
  Json.

unpopulate_response(#queue_update{time = [A, B, C], messages = Rpcs}) ->
  { [ { <<"method">>, <<"queue_update">>},
      { <<"when">>, [A, B, C]},
      { <<"messages">>, [unpopulate_response(X) || X <- Rpcs]}] };

unpopulate_response(Record)
    when is_tuple(Record)
    andalso is_atom(element(1, Record)) ->
  RecordName = element(1, Record),
  Fields = '#info-'(RecordName),
  { [ { <<"method">>,
        atom_to_binary(RecordName, utf8) } | 

       [  { atom_to_binary(Field, utf8),
            Value } || Field <- Fields,
                                        Value <- ['#get-'(Field, Record)],
                                        Value =/= undefined]
  ] };

unpopulate_response(Json) ->
  Json.


