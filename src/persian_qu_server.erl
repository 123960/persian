-module(persian_qu_server).
-behaviour(gen_server).
-import(persian_event_server, [notify_new_msg/2, notify_no_msg/2, process_msg/3]).
-export([init/1, terminate/2, start/0, code_change/3, handle_call/3, handle_cast/2, handle_info/2, stop/1,
         sync_enqueue/3, sync_get_msgs/2, sync_dequeue/2, async_dequeue/2]).

init([]) ->
  io:format("[persian_qu_server] - Iniciando qu_server.\n"), 
  persian_event_server:start(),
  {ok, []}.

%%------------------------- Module API ---------------------------------
start()                        -> gen_server:start_link({local, persian_qu_server}, ?MODULE, [], []).
stop(Pid)                      -> gen_server:call(Pid, {terminate}).
sync_enqueue(Pid, Client, Msg) -> gen_server:call(Pid, {enq, Client, Msg}).
sync_get_msgs(Pid, Client)     -> gen_server:call(Pid, {get, Client}).
sync_dequeue(Pid, Client)      -> gen_server:call(Pid, {deq, Client}).
async_dequeue(Pid, Client)     -> gen_server:cast(Pid, {deq, Client}).

%%--------------------- Enqueue handle_call ----------------------------
handle_call({enq, Client, Msg}, _From, []) ->
  self() ! {enq, Client, Msg},
  MapQueue = orddict:store(Client, enqueue(Client, Msg, []), orddict:new()),
  {reply, ok, MapQueue};
handle_call({enq, Client, Msg}, _From, MapQueue) ->
  self() ! {enq, Client, Msg},
  Queue = enqueue(Client, Msg, MapQueue),
  {reply, ok, orddict:store(Client, Queue, MapQueue)};

%%--------------------- GET handle_call --------------------------------
handle_call({get, Client}, _From, []) ->
  self() ! {get, Client},
  {reply, no_msg, []};
handle_call({get, Client}, _From, MapQueue) ->
  self() ! {get, Client},
  case orddict:find(Client, MapQueue) of
    {ok, {[],[]}} -> {reply, {no_msg, Client}, MapQueue};
    {ok, Q}       -> {reply, Q, MapQueue}
  end;

%%--------------------- Dequeue handle_call ----------------------------
handle_call({deq, Client}, _From, MapQueue) ->
  self() ! {deq, Client},
  case dequeue(Client, MapQueue) of
    {{value, Item}, Q2} -> {reply, Item, orddict:store(Client, Q2, MapQueue)};
    no_msg              -> {reply, {no_msg, Client}, MapQueue}
  end;

%%--------------------- STOP handle_call --------------------------------
handle_call({terminate}, _From, MapQueue) ->
  {stop, normal, ok, MapQueue}.

%%--------------------- Dequeue handle_cast (async) ---------------------
handle_cast({deq, Client}, MapQueue) ->
  self() ! {async_deq, Client},
  case dequeue(Client, MapQueue) of
    {{value, Item}, Q2} -> process_msg(Client, Item),
                           {noreply, orddict:store(Client, Q2, MapQueue)};
    no_msg              -> notify_no_msg(Client),
                           {noreply, MapQueue}
  end.

%%--------------------- handle_info -------------------------------------
handle_info(Msg, MapQueue) ->
  io:format("[persian_qu_server] - Receive message: ~p~n",[Msg]),
  {noreply, MapQueue}.

%%--------------------- CODE_CHANGE handle ------------------------------
code_change(PreviousVersion, State, Extra) ->
  self() ! {code_change, PreviousVersion, State, Extra},
  {ok, State}.

%%--------------------- terminate  --------------------------------------
terminate(normal, _MapQueue) ->
  io:format("[persian_qu_server] - Solicitando a parada do event_server...\n"),
  persian_event_server:stop(whereis(persian_event_server)),
  io:format("[persian_qu_server] - Encerrando qu_server.\n"),
  ok.

%%--------------------- Private functions -------------------------------
%%---- ENQUEUE ------------------------------------
enqueue(Msg, [])    -> queue:in(Msg, queue:new());
enqueue(Msg, Queue) -> queue:in(Msg, Queue).
enqueue(Client, Msg, MapQueue) ->
  case orddict:find(Client, MapQueue) of
    {ok, Q} -> notify_new_msg(Client),
               enqueue(Msg, Q);
    error   -> notify_new_msg(Client),
               enqueue(Msg, [])
  end.

%%---- DEQUEUE ------------------------------------
dequeue(Client, MapQueue) ->
  case orddict:find(Client, MapQueue) of
    {ok, Q} -> case queue:out(Q) of
                 {empty, _}          -> no_msg;
                 {{value, Item}, Q2} -> {{value, Item}, Q2}
               end;
    error   -> no_msg
  end.

%%---- NEW_MSG ------------------------------------
notify_new_msg(Client) ->
  persian_event_server:notify_new_msg(whereis(persian_event_server), Client).
%%---- NO_MSG ------------------------------------
notify_no_msg(Client) ->
  persian_event_server:notify_no_msg(whereis(persian_event_server), Client).
%%---- PROCESS_MSG ------------------------------------
process_msg(Client, Msg) ->
  persian_event_server:process_msg(whereis(persian_event_server), Client, Msg).
