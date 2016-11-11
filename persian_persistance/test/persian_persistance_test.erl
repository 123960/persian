werl -sname persian_persistance_node0@localhost -pa _build\default\lib\persian_persistance\ebin _build\default\lib\goldrush\ebin _build\default\lib\lager\ebin
erl -sname persian_persistance_node0@localhost -pa _build/default/lib/*/ebin
application:start(persian_persistance).

persian_persistance:create_db([]).
persian_persistance:get_queue(1).
