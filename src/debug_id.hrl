debug_id(Message) ->
  io:write(lists:flatten(io_lib:fwrite("~p@~p reports ~p~n", [?MODULE, self(), Message]))).

