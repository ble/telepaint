-module(rpc_methods).

-compile({parse_transform, exprecs}).
-export([populate/2, unpopulate/1, unpopulate_response/1]).
-export_records([set_name, chat]).

-record(set_name,
  { who = self,
    name = undefined }).

-record(chat,
  { who = self,
    message = undefined }).

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

unpopulate(Record) ->
  RecordName = element(1, Record),
  Fields = '#info-'(RecordName),
  { [{ atom_to_binary(Field, utf8),
       '#get-'(Field, Record) } || Field <- Fields]}.

unpopulate_response(Record) ->
  RecordName = element(1, Record),
  Fields = '#info-'(RecordName),
  { [ { <<"method">>,
        atom_to_binary(RecordName, utf8) } | 

       [  { atom_to_binary(Field, utf8),
            '#get-'(Field, Record) } || Field <- Fields ]
  ] }.


