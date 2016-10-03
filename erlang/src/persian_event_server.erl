-module(persian_event_server).
-behaviour(gen_server).
-import(persian_qu_server, [async_dequeue/2]).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3,
terminate/2, start/0, stop/1, notify_new_msg/2, notify_no_msg/2, process_msg/3]).

init([]) ->
  io:format("[persian_event_server] - Iniciando event_server.\n"),
  {ok, []}.

%%--------------------- Module API --------------------------------------
start()                        -> gen_server:start_link({local, persian_event_server}, ?MODULE, [], []).
stop(Pid)                      -> gen_server:call(Pid, {terminate}).
notify_new_msg(Pid, Client)    -> gen_server:cast(Pid, {new_msg, Client}).
notify_no_msg(Pid, Client)     -> gen_server:cast(Pid, {no_msg, Client}).
process_msg(Pid, Client, Msg)  -> gen_server:cast(Pid, {process_msg, Client, Msg}).

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
handle_cast({process_msg, Client, Msg}, State) ->
  self() ! {process_msg, Client},
  io:format("[persian_event_server] - Processing msg: ~p~n",[Msg]),
  persian_qu_server:async_dequeue(whereis(persian_qu_server), Client),
  {noreply, State}.

%%--------------------- handle_call ----------------------------

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

%%--------------------- Private functions -------------------------------
