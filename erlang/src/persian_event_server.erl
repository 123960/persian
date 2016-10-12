-module(persian_event_server).
-behaviour(gen_server).
-import(persian_qu_server, [async_dequeue/2]).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3,
         terminate/2, start_link/0, stop/1, get_all_msgs/1, get_client_msgs/2, get_msg/3,
         notify_new_msg/2, notify_no_msg/2, process_msg/3, store_resp/4]).

init([]) ->
  io:format("[persian_event_server] - Iniciando event_server.~n"),
  {ok, []}.

%%====================================================================
%% API functions
%%====================================================================
start_link()                         -> gen_server:start_link({local, persian_event_server}, ?MODULE, [], []).
stop(Pid)                            -> gen_server:call(Pid, {terminate}).
get_all_msgs(Pid)                    -> gen_server:call(Pid, {get_all_msgs}).
get_client_msgs(Pid, Client)         -> gen_server:call(Pid, {get_client_msgs, Client}).
get_msg(Pid, Client, MsgId)          -> gen_server:call(Pid, {get_msg, Client, MsgId}).
notify_new_msg(Pid, Client)          -> gen_server:cast(Pid, {new_msg, Client}).
notify_no_msg(Pid, Client)           -> gen_server:cast(Pid, {no_msg, Client}).
process_msg(Pid, Client, Msg)        -> gen_server:cast(Pid, {process_msg, Client, Msg}).
store_resp(Pid, Client, MsgId, Resp) -> gen_server:cast(Pid, {store_resp, Client, MsgId, Resp}).

%%====================================================================
%% Callback Handlers
%%====================================================================
%%--------------------- NEW_MSG handle_cast (async) --------------------
handle_cast({new_msg, Client}, State) ->
  self() ! {new_msg, Client},
  persian_qu_server:async_dequeue(whereis(persian_qu_server), Client),
  {noreply, State};

%%--------------------- NO_MSG handle_cast (async) --------------------
handle_cast({no_msg, Client}, State) ->
  self() ! {no_msg, Client},
  {noreply, State};

%%--------------------- PROCESS_MSG handle_cast (async) --------------------
handle_cast({process_msg, Client, {MsgId, Msg}}, State) ->
  self() ! {process_msg, Client},
  io:format("[persian_event_server][process_msg][MsgId:[~p]] - Processando mensagem~n", [MsgId]),
  io:format("[persian_event_server][process_msg][MsgId:[~p]] - Procurando mensagem no cache.~n", [MsgId]),
  case State of
    []    -> io:format("[persian_event_server][process_msg][MsgId:[~p]] - Cache vazio, enviando mensagem...~n", [MsgId]),
             process_event(Client, MsgId, Msg),
             request_new_msg(Client),
             {noreply, orddict:store(Client, orddict:store(MsgId, [{sent, ok, get_timestamp()}], orddict:new()), State)};
    _else -> case orddict:find(Client, State) of
               error      -> io:format("[persian_event_server][process_msg][MsgId:[~p]] - Nao ha mensagens para o cliente, enviando mensagem...~n", [MsgId]),
                             process_event(Client, MsgId, Msg),
                             request_new_msg(Client),
                             {noreply, orddict:store(Client, orddict:store(MsgId, [{sent, ok, get_timestamp()}], orddict:new()), State)};
               {ok, Msgs} -> case orddict:find(MsgId, Msgs) of
                               error   -> io:format("[persian_event_server][process_msg][MsgId:[~p]] - Mensagen nao processada, enviando mensagem...~n", [MsgId]),
                                          process_event(Client, MsgId, Msg),
                                          request_new_msg(Client),
                                          {noreply, orddict:store(Client, orddict:store(MsgId, [{sent, ok, get_timestamp()}], Msgs), State)};
                               {ok, _} -> io:format("[persian_event_server][process_msg][MsgId:[~p]] - Mensagen ja processada.~n", [MsgId]),
                                          request_new_msg(Client),
                                          {noreply, State}
                             end
             end
  end;

handle_cast({store_resp, Client, MsgId, _Resp}, State) ->
  self() ! {store_resp, Client},
  io:format("[persian_event_server][store_resp][MsgId:[~p]|Client:[~p]] - Processando resposta para mensagem.~n", [MsgId, Client]),
  case State of
    []    -> io:format("[persian_event_server][process_msg][MsgId:[~p]] - Cache vazio, something is wrong...~n", [MsgId]),
             {noreply, State};
    _else -> case orddict:find(Client, State) of
               error      -> io:format("[persian_event_server][process_msg][MsgId:[~p]] - Nao ha mensagens para o cliente, something is wrong...~n", [MsgId]),
                             {noreply, State};
               {ok, Msgs} -> case orddict:find(MsgId, Msgs) of
                               error         ->
                                 io:format("[persian_event_server][process_msg][MsgId:[~p]] - Mensagen nao processada, something is wrong...~n", [MsgId]),
                                 {noreply, State};
                               {ok, MsgInfo} ->
                                 io:format("[persian_event_server][process_msg][MsgId:[~p]] - Registrando resposta.~n", [MsgId]),
                                 {noreply, orddict:store(Client, orddict:store(MsgId, lists:append(MsgInfo, [{resp, ok, get_timestamp()}]), Msgs), State)}
                             end
             end
  end.

%%--------------------- handle_call ----------------------------
handle_call({get_all_msgs}, _From, State) ->
  {reply, State, State};
handle_call({get_client_msgs, Client}, _From, State) ->
  case State of
    []    -> {reply, State, State};
    _else -> case orddict:find(Client, State) of
               error      -> {reply, [], State};
               {ok, Msgs} -> {reply, Msgs, State}
             end
  end;
handle_call({get_msg, Client, MsgId}, _From, State) ->
  case State of
    []    -> {reply, State, State};
    _else -> case orddict:find(Client, State) of
               error      -> {reply, [], State};
               {ok, Msgs} -> case orddict:find(MsgId, Msgs) of
                               error     -> {reply, [], State};
                               {ok, Msg} -> {reply, Msg, State}
                             end
             end
  end;
%%--------------------- STOP handle_call --------------------------------
handle_call({terminate}, _From, State) ->
  {stop, normal, ok, State}.

%%--------------------- handle_info -------------------------------------
handle_info(Msg, State) ->
  io:format("[persian_event_server][Msg:[~p]] - Receive message.~n", [Msg]),
  {noreply, State}.

%%--------------------- CODE_CHANGE handle ------------------------------
code_change(PreviousVersion, State, Extra) ->
  self() ! {code_change, PreviousVersion, State, Extra},
  {ok, State}.

%%--------------------- terminate  --------------------------------------
terminate(normal, _State) ->
  io:format("[persian_event_server] - Encerrando event_server.~n"),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
request_new_msg(Client) ->
  persian_qu_server:async_dequeue(whereis(persian_qu_server), Client).
process_event(Client, MsgId, Msg) ->
  persian_httpc_acm_server:process_event(whereis(persian_httpc_acm_server), Client, MsgId, Msg).
get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).
