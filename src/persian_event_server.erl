-module(persian_event_server).
-behaviour(gen_server).
-import(persian_qu_server, [async_dequeue/2]).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3,
         terminate/2, start/0, stop/1, get_all_msgs/1, get_client_msgs/2, get_msg/3,
         notify_new_msg/2, notify_no_msg/2, process_msg/3]).

init([]) ->
  io:format("[persian_event_server] - Iniciando event_server.\n"),
  {ok, []}.

%%====================================================================
%% API functions
%%====================================================================
start()                        -> gen_server:start_link({local, persian_event_server}, ?MODULE, [], []).
stop(Pid)                      -> gen_server:call(Pid, {terminate}).
get_all_msgs(Pid)              -> gen_server:call(Pid, {get_all_msgs}).
get_client_msgs(Pid, Client)   -> gen_server:call(Pid, {get_client_msgs, Client}).
get_msg(Pid, Client, MsgId)    -> gen_server:call(Pid, {get_msg, Client, MsgId}).
notify_new_msg(Pid, Client)    -> gen_server:cast(Pid, {new_msg, Client}).
notify_no_msg(Pid, Client)     -> gen_server:cast(Pid, {no_msg, Client}).
process_msg(Pid, Client, Msg)  -> gen_server:cast(Pid, {process_msg, Client, Msg}).

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
handle_cast({process_msg, Client, {Id, _Content}}, State) ->
  self() ! {process_msg, Client},
  io:format("[persian_event_server] - Processing msg com id ~p~n",[Id]),
  io:format("[persian_event_server] - Procurando mensagem no cache.~n"),
  case State of
    []    -> io:format("[persian_event_server] - Cache vazio, enviando mensagem...~n"),
             persian_qu_server:async_dequeue(whereis(persian_qu_server), Client),
             {noreply, orddict:store(Client, orddict:store(Id, ok, orddict:new()), State)};
    _else -> case orddict:find(Client, State) of
               error      -> io:format("[persian_event_server] - Nao ha mensagens para o cliente, enviando mensagem...~n"),
                             persian_qu_server:async_dequeue(whereis(persian_qu_server), Client),
                             {noreply, orddict:store(Client, orddict:store(Id, ok, orddict:new()), State)};
               {ok, Msgs} -> case orddict:find(Id, Msgs) of
                               error   -> io:format("[persian_event_server] - Mensagen nao processada, enviando mensagem...~n"),
                                          persian_qu_server:async_dequeue(whereis(persian_qu_server), Client),
                                          {noreply, orddict:store(Client, orddict:store(Id, ok, Msgs), State)};
                               {ok, _} -> io:format("[persian_event_server] - Mensagen ja processada.~n"),
                                          persian_qu_server:async_dequeue(whereis(persian_qu_server), Client),
                                          {noreply, State}
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
  io:format("[persian_event_server] - Receive message: ~p~n",[Msg]),
  {noreply, State}.

%%--------------------- CODE_CHANGE handle ------------------------------
code_change(PreviousVersion, State, Extra) ->
  self() ! {code_change, PreviousVersion, State, Extra},
  {ok, State}.

%%--------------------- terminate  --------------------------------------
terminate(normal, _State) ->
  io:format("[persian_event_server] - Encerrando event_server.\n"),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
