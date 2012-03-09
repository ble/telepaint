-record(rpc_call,
  { version = <<"2.0">>,
    method = <<"unspecified_method">>, % :: binary(),
    params = [], % :: [term()],
    id = undefined% :: undefined | binary() | integer()
  }).

-record(rpc_response,
  { version = <<"2.0">>,
    result = undefined,% :: term(),
    error = undefined,% :: term(),
    id = undefined% :: undefined | binary() | integer()
  }).

-record(rpc_response_error,
  { code = -1,% :: integer(),
    message = <<"undescribed error">>,% :: binary(),
    data = undefined% :: undefined | term()
  }).

