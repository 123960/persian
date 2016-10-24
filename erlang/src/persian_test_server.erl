-module(persian_test_server).
-export([handle/2, handle_event/3]).
-include_lib("elli/include/elli.hrl").
-behaviour(elli_handler).

handle(Req, _Args) ->
%% Delegate to our handler function
handle(Req#req.method, elli_request:path(Req), Req).

%%====================================================================
%% Helloworld operations
%%====================================================================
handle('GET',[<<"persian">>, <<"hello">>], _Req) ->
  {ok, [], <<"Hello World!">>};
handle('GET',[<<"persian">>, <<"test">>, <<"ok">>], _Req) ->
  {ok, [{<<"Result">>, <<"Success">>}, {<<"Code">>, <<"Success">>}], <<"Hello World!">>};
handle('GET',[<<"persian">>, <<"test">>, <<"nok">>], _Req) ->
  {ok, [{<<"Result">>, <<"Error">>}, {<<"Code">>, <<"Error-0000">>}], <<"Hello World!">>};
handle('POST',[<<"persian">>, <<"test">>, <<"ok">>], _Req) ->
  {ok, [{<<"Result">>, <<"Success">>}, {<<"Code">>, <<"Success">>}], <<"Hello World!">>};
handle('POST',[<<"persian">>, <<"test">>, <<"nok">>], _Req) ->
  {ok, [{<<"Result">>, <<"Error">>}, {<<"Code">>, <<"Error-0000">>}], <<"Hello World!">>};
handle(_, _, _Req) ->
  {404, [], <<"Not Found">>}.

%% @doc: Handle request events, like request completed, exception
%% thrown, client timeout, etc. Must return 'ok'.
handle_event(_Event, _Data, _Args) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
