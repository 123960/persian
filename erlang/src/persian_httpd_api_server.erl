-module(persian_httpd_api_server).
-export([handle/2, handle_event/3]).
-import(persian_qu_server, [sync_enqueue/3, sync_get_msgs/2]).
-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
  %% Delegate to our handler function
  handle(Req#req.method, elli_request:path(Req), Req).

handle('GET',[<<"persian">>, <<"helloworld">>], _Req) ->
  {ok, [], <<"Hello World!">>};
handle('POST',[<<"persian">>, <<"helloworld">>], _Req) ->
  {ok, [], <<"Hello World!">>};

handle('POST',[<<"persian">>, <<"enqueue">>], Req) ->
  Client = elli_request:get_arg(<<"client">>, Req, <<"undefined">>),
  MsgId  = elli_request:get_arg(<<"msgId">>, Req, <<"undefined">>),
  Msg    = elli_request:body(Req),
  persian_qu_server:sync_enqueue(whereis(persian_qu_server), Client, {MsgId, Msg}),
  {ok, [], <<"enqueue ok for message ", MsgId/binary, " of client ", Client/binary>>};

handle(_, _, _Req) ->
  {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
  ok.
