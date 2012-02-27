
-record(set_name,
  { who = self,
    name = undefined }).

-record(chat,
  { who = self,
    message = undefined }).

-record(join_room,
  { who = undefined,
    name = undefined }).

-record(queue_update,
  { time = [0, 0, 0],
    messages = [] }).
