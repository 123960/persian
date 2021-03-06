-module(persian_httpd_api_server).
-compile([{parse_transform, lager_transform}]).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
%% Delegate to our handler function
handle(Req#req.method, elli_request:path(Req), Req).

%%====================================================================
%% Helloworld operations
%%====================================================================
handle('GET',[<<"persian">>, <<"helloworld">>], _Req) ->
  {ok, [], <<"Hello World!">>};
handle('POST',[<<"persian">>, <<"helloworld">>], _Req) ->
  {ok, [], <<"Hello World!">>};

%%====================================================================
%% GET sync_operations
%%====================================================================
handle('GET',[<<"persian">>, <<"sync">>, <<"get_pend_msgs">>], _Req) ->
  MQ = sync_get_msgs(),
  {ok, [], persian_json:mq_to_json(MQ)};
handle('GET',[<<"persian">>, <<"sync">>, <<"get_pend_msgs_of_client">>], Req) ->
  Client = elli_request:get_arg(<<"client">>, Req, <<"undefined">>),
  MQ     = sync_get_msgs(Client),
  {ok, [], persian_json:mq_to_json(MQ)};
handle('GET',[<<"persian">>, <<"sync">>, <<"get_processed_msgs">>], _Req) ->
  ME = get_all_msgs(),
  {ok, [], persian_json:event_to_json(ME)};
handle('GET',[<<"persian">>, <<"sync">>, <<"get_processed_msgs_of_client">>], Req) ->
  Client = elli_request:get_arg(<<"client">>, Req, <<"undefined">>),
  ME     = get_client_msgs(Client),
  {ok, [], persian_json:event_to_json(ME)};
handle('GET',[<<"persian">>, <<"sync">>, <<"get_error_msgs">>], _Req) ->
  {ok, [], <<"NOT IMPLEMENTED YET!">>};
handle('GET',[<<"persian">>, <<"sync">>, <<"get_error_msgs_of_client">>], _Req) ->
  {ok, [], <<"NOT IMPLEMENTED YET!">>};
handle('GET',[<<"persian">>, <<"sync">>, <<"get_msg">>], _Req) ->
  {ok, [], <<"NOT IMPLEMENTED YET!">>};

%%====================================================================
%% POST sync_operations
%%====================================================================
handle('POST',[<<"persian">>, <<"sync">>, <<"enqueue">>], Req) ->
  Client = elli_request:get_arg(<<"client">>, Req, <<"undefined">>),
  MsgId  = elli_request:get_arg(<<"msgId">>, Req, <<"undefined">>),
  Msg    = elli_request:body(Req),
  sync_enqueue(Client, MsgId, Msg),
  {ok, [], <<"enqueue ok for message ", MsgId/binary, " of client ", Client/binary>>};

handle(_, _, _Req) ->
  {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
sync_get_msgs() ->
  persian_qu_server:sync_get_msgs(persian_qu_nodem:qu_server_name()).

sync_get_msgs(Client) ->
  persian_qu_server:sync_get_msgs(persian_qu_nodem:qu_server_name(), Client).

get_all_msgs() ->
  persian_event_server:get_all_msgs(event_server_name(0)).

get_client_msgs(Client) ->
  persian_event_server:get_client_msgs(event_server_name(Client), Client).

sync_enqueue(Client, MsgId, Msg) ->
  persian_qu_server:sync_enqueue(persian_qu_nodem:qu_server_name(), Client, {MsgId, Msg}).

event_server_name(Client) ->
  persian_qu_nodem:event_server_name(persian_qu_nodem:event_node(Client)).
