c(persian_qu_server).
f(Pid).

{ok, Pid} = persian_qu_server:start().
persian_qu_server:sync_enqueue(Pid, "client1", {"msg1", 1}).
persian_qu_server:sync_enqueue(Pid, "client1", {"msg2", 2}).
persian_qu_server:sync_enqueue(Pid, "client1", {"msg3", 3}).
persian_qu_server:sync_enqueue(Pid, "client2", {"msg4", 4}).
persian_qu_server:sync_enqueue(Pid, "client2", {"msg5", 5}).
persian_qu_server:sync_enqueue(Pid, "client2", {"msg6", 6}).
persian_qu_server:sync_get_msgs(Pid, 1).
persian_qu_server:stop(Pid).

Clients = lists:seq(1, 10).
Msgs    = lists:seq(1, 100).
X       = lists:flatmap(fun(C) -> [{C, M} || M <- Msgs] end, Clients).
[persian_qu_server:sync_enqueue(Pid, C, M) || {C, M} <- X].

{ok, Pid} = elli:start_link([{callback, persian_http_api_server}, {port, 3000}]).

EPid = whereis(persian_event_server).
persian_event_server:get_all_msgs(EPid).
persian_event_server:get_client_msgs(EPid, <<"1">>).
persian_event_server:get_msg(EPid, "client1", "msg1").

application:start(persian).
application:stop(persian).
persian_supersup:start_link().
supervisor:which_children(persian_qu_server_sup).
{ok, Pid} = elli:start_link([{callback, persian_http_api_server}, {port, 3000}]).

Pid = whereis(persian_qu_server).

erl -pa _build\default\lib\elli\ebin _build\default\lib\persian\ebin
erl -pa _build/default/lib/*/ebin

http://127.0.0.1:3000/persian/enqueue?msgId=1&client=1

application:start(persian).
application:stop(persian).
