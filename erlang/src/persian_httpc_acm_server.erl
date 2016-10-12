-module(persian_httpc_acm_server).
-behaviour(gen_server).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3,
         terminate/2, start_link/0, stop/1, process_event/4]).

-define(ACM_URL, "http://172.22.4.142:8080/genericadapter/GenericAdapter").

init([]) ->
  io:format("[persian_httpc_acm_server] - Iniciando inets.\n"),
  application:start(inets),
  io:format("[persian_httpc_acm_server] - Iniciando persian_httpc_acm_server.\n"),
  {ok, []}.

%%====================================================================
%% API functions
%%====================================================================
start_link()                           -> gen_server:start_link({local, persian_httpc_acm_server}, ?MODULE, [], []).
stop(Pid)                              -> gen_server:call(Pid, {terminate}).
process_event(Pid, Client, MsgId, Msg) -> gen_server:call(Pid, {process_event, Client, MsgId, Msg}).

%%====================================================================
%% Callback Handlers
%%====================================================================
%%--------------------- NEW_MSG handle_cast (async) --------------------
handle_cast({new_msg, Client}, State) ->
  self() ! {new_msg, Client},
  {noreply, State}.

%%--------------------- handle_call ----------------------------
handle_call({process_event, Client, MsgId, Msg}, _From, State) ->
  io:format("[persian_httpc_acm_server][client:[~p]|msgid:[~p]] - Executando HTTP Request~n", [Client, MsgId]),
  case httpc:request(post, {?ACM_URL, [], [], Msg}, [], []) of
    {ok, Result}    -> io:format("[persian_httpc_acm_server][client:[~p]|msgid:[~p]] - Retorno OK do HTTP Request, iniciando o tratamento do response...~n", [Client, MsgId]),
                       send_resp(Client, MsgId, Result);
    {error, Reason} -> io:format("[persian_httpc_acm_server][client:[~p]|msgid:[~p]] - Retorno NOK do HTTP Request, nao vou fazer nada.~n", [Client, MsgId]),
                       io:format("[persian_httpc_acm_server][client:[~p]|msgid:[~p]] - Reason: ~p~n", [Client, MsgId, Reason])
  end,
  {reply, State, State};

%%--------------------- STOP handle_call --------------------------------
handle_call({terminate}, _From, State) ->
  {stop, normal, ok, State}.

%%--------------------- handle_info -------------------------------------
handle_info(Msg, State) ->
  io:format("[persian_httpc_acm_server][msgId:[~p~n]] - Receive message",[Msg]),
  {noreply, State}.

%%--------------------- CODE_CHANGE handle ------------------------------
code_change(PreviousVersion, State, Extra) ->
  self() ! {code_change, PreviousVersion, State, Extra},
  {ok, State}.

%%--------------------- terminate  --------------------------------------
terminate(normal, _State) ->
  io:format("[persian_httpc_acm_server] - Encerrando inets.~n"),
  application:stop(inets),
  io:format("[persian_httpc_acm_server] - Encerrando persian_httpc_acm_server.~n"),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
send_resp(Client, MsgId, Resp) ->
  persian_event_server:store_resp(whereis(persian_event_server), Client, MsgId, Resp).
