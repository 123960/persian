c(persian_qu_server).
f(Pid).

{ok, APid} = elli:start_link([{callback, persian_httpd_api_server}, {port, 3000}]).
{ok, QPid} = persian_qu_server:start_link().
{ok, EPid} = persian_event_server:start_link().
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

EPid = whereis(persian_event_server).
persian_event_server:get_all_msgs(EPid).
persian_event_server:get_client_msgs(EPid, "client2").
persian_event_server:get_msg(EPid, "client1", "msg1").

QPid = whereis(persian_qu_server).
persian_qu_server:sync_get_msgs(QPid).
persian_qu_server:sync_get_msgs(QPid, 1).

application:start(persian).
application:stop(persian).
persian_supersup:start_link().
supervisor:which_children(persian_qu_server_sup).
{ok, Pid} = elli:start_link([{callback, persian_http_api_server}, {port, 3000}]).

Pid = whereis(persian_qu_server).

erl -pa _build\default\lib\elli\ebin _build\default\lib\persian_qu\ebin _build\default\lib\goldrush\ebin _build\default\lib\jsone\ebin _build\default\lib\lager\ebin
erl -pa _build/default/lib/*/ebin

werl -sname persian_qu@localhost -pa _build\default\lib\elli\ebin _build\default\lib\persian_qu\ebin _build\default\lib\goldrush\ebin _build\default\lib\jsone\ebin _build\default\lib\lager\ebin
werl -sname persian_event@localhost -pa _build\default\lib\persian_event\ebin _build\default\lib\goldrush\ebin _build\default\lib\lager\ebin

http://127.0.0.1:3000/persian/enqueue?msgId=1&client=1

application:start(persian).
application:stop(persian).

%%--------json--------
persian_json:to_json([]).
persian_json:to_json({[],[]}).
persian_json:to_json({[1],[]}).
persian_json:to_json({[2],[1]}).
persian_json:to_json({[3,2],[1]}).
persian_json:to_json({[4,3,2],[1]}).

persian_json:to_json([]).
persian_json:to_json([{<<"client1">>,{[],[]}}]).
persian_json:to_json([{<<"client1">>,{[1],[]}}]).
persian_json:to_json([{<<"client1">>,{[2],[1]}}]).
persian_json:to_json([{<<"client1">>,{[3,2],[1]}}]).
persian_json:to_json([{<<"client1">>,{[4,3,2],[1]}}, {<<"client2">>,{[4,3,2],[1]}}]).

persian_json:mqu_to_mlist([{<<"client1">>,{[],[]}}]).
persian_json:mqu_to_mlist([{<<"client1">>,{[1],[]}}]).
persian_json:mqu_to_mlist([{<<"client1">>,{[2],[1]}}]).
persian_json:mqu_to_mlist([{<<"client1">>,{[3,2],[1]}}]).
persian_json:mqu_to_mlist([{<<"client1">>,{[4,3,2],[1]}}]).
persian_json:mqu_to_mlist([{<<"client1">>,{[4,3,2],[1]}}, {<<"client2">>,{[4,3,2],[1]}}]).
