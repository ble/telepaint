
-ifdef(debug).
debug_msg(Format, Items) ->
  io:format(Format, Items).
-else.
debug_msg(_Format, _Items) ->
  noop.
-endif.

