c(persian_qu_server).
f(Pid).
{ok, Pid} = persian_qu_server:start().
persian_qu_server:sync_enqueue(Pid, 1, 2).
persian_qu_server:sync_enqueue(Pid, 1, 3).
persian_qu_server:sync_enqueue(Pid, 1, 4).
persian_qu_server:sync_enqueue(Pid, 2, 5).
persian_qu_server:sync_enqueue(Pid, 2, 6).
persian_qu_server:sync_enqueue(Pid, 2, 7).
persian_qu_server:sync_get_msgs(Pid, 1).
persian_qu_server:sync_get_msgs(Pid, 2).
persian_qu_server:sync_dequeue(Pid, 1).
persian_qu_server:stop(Pid).

%% no_msg - empty state
f(Pid).
{ok, Pid} = persian_qu_server:start().
persian_qu_server:sync_get_msgs(Pid, 1).
persian_qu_server:sync_dequeue(Pid, 1).
persian_qu_server:stop(Pid).

%% no_msg - dequeue all
f(Pid).
{ok, Pid} = persian_qu_server:start().
persian_qu_server:sync_enqueue(Pid, 1, 2).
persian_qu_server:sync_enqueue(Pid, 1, 3).
persian_qu_server:sync_dequeue(Pid, 1).
persian_qu_server:sync_dequeue(Pid, 1).
persian_qu_server:sync_dequeue(Pid, 1).
persian_qu_server:sync_get_msgs(Pid, 1).
persian_qu_server:stop(Pid).
