-module(persian_qu_server).
-behaviour(gen_server).
-export([init/1, terminate/2, start/0, code_change/3, handle_call/3, handle_info/2, stop/1,
         sync_enqueue/3, sync_get_msgs/2, sync_dequeue/2]).

init([]) -> {ok, []}.

%%--------------------- Module API ----------------------------
start()                        -> gen_server:start_link(?MODULE, [], []).
stop(Pid)                      -> gen_server:call(Pid, {terminate}).
sync_enqueue(Pid, Client, Msg) -> gen_server:call(Pid, {enq, Client, Msg}).
sync_get_msgs(Pid, Client)     -> gen_server:call(Pid, {get, Client}).
sync_dequeue(Pid, Client)      -> gen_server:call(Pid, {deq, Client}).

%%--------------------- Enqueue handle_call ----------------------------
handle_call({enq, Client, Msg}, _From, []) ->
  self() ! {enq, Client, Msg},
  MapQueue = orddict:store(Client, enqueue(Msg, []), orddict:new()),
  {reply, ok, MapQueue};
handle_call({enq, Client, Msg}, _From, MapQueue) ->
  self() ! {enq, Client, Msg},
  Queue = enqueue(Client, Msg, MapQueue),
  {reply, ok, orddict:store(Client, Queue, MapQueue)};

%%--------------------- GET handle_call ----------------------------
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

%%--------------------- STOP handle_call ----------------------------
handle_call({terminate}, _From, MapQueue) ->
  {stop, normal, ok, MapQueue}.

%%--------------------- INFO handle ----------------------------
handle_info(Msg, MapQueue) ->
  io:format("Receive message: ~p~n",[Msg]),
  {noreply, MapQueue}.

%%--------------------- CODE_CHANGE handle ----------------------------
code_change(PreviousVersion, State, Extra) ->
  self() ! {code_change, PreviousVersion, State, Extra},
  {ok, State}.

%%--------------------- terminate  ----------------------------
terminate(normal, _MapQueue) ->
  io:format("Encerrando...\n"),
  ok.

%%--------------------- Private function ----------------------------
enqueue(Msg, [])    -> queue:in(Msg, queue:new());
enqueue(Msg, Queue) -> queue:in(Msg, Queue).
enqueue(Client, Msg, MapQueue) ->
  case orddict:find(Client, MapQueue) of
    {ok, Q} -> enqueue(Msg, Q);
    error   -> enqueue(Msg, [])
  end.
dequeue(Client, MapQueue) ->
  case orddict:find(Client, MapQueue) of
    {ok, Q} -> case queue:out(Q) of
                 {empty, _}          -> no_msg;
                 {{value, Item}, Q2} -> {{value, Item}, Q2}
               end;
    error   -> no_msg
  end.
