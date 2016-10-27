%%%-------------------------------------------------------------------
%% @doc persian qu_server.
%% State holds messages waiting for dequeue in a map by client
%% MapQueue = [{Client1, [{MsgId1, MsgContent1}, {MsgId2, MsgContent2}...]},
%%             {Client2, [{MsgId1, MsgContent1}, {MsgId2, MsgContent2}...]}
%%              ...]
%% @end
%%%-------------------------------------------------------------------
-module(persian_qu_server).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

-export([init/1, terminate/2, start_link/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2, stop/1,
         sync_enqueue/3, sync_get_msgs/1, sync_get_msgs/2, sync_dequeue/2, async_dequeue/2]).

init([]) ->
  lager:info("- Starting persian_qu_server"),
  {ok, orddict:new()}.

%%====================================================================
%% API functions
%%====================================================================
start_link(PName)                -> gen_server:start_link({global, PName}, ?MODULE, [], []).
stop(PName)                      -> gen_server:call({global, PName}, {terminate}).
sync_enqueue(PName, Client, Msg) -> gen_server:call({global, PName}, {enq, Client, Msg}).
sync_get_msgs(PName)             -> gen_server:call({global, PName}, {get_all_msgs}).
sync_get_msgs(PName, Client)     -> gen_server:call({global, PName}, {get, Client}).
sync_dequeue(PName, Client)      -> gen_server:call({global, PName}, {deq, Client}).
async_dequeue(PName, Client)     -> gen_server:cast({global, PName}, {deq, Client}).

%%====================================================================
%% Callback Handlers
%%====================================================================
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
handle_call({get_all_msgs}, _From, MapQueue) ->
  self() ! {get_all_msgs},
  {reply, MapQueue, MapQueue};
handle_call({get, Client}, _From, []) ->
  self() ! {get, Client},
  {reply, [], []};
handle_call({get, Client}, _From, MapQueue) ->
  self() ! {get, Client},
  case orddict:find(Client, MapQueue) of
    {ok, Q}       -> {reply, orddict:store(Client, Q, orddict:new()), MapQueue};
    error         -> {reply, orddict:store(Client, queue:new(), orddict:new()), MapQueue}
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
  lager:info("- [Msg:[~p]] - Receive message", [Msg]),
  {noreply, MapQueue}.

%%--------------------- CODE_CHANGE handle ------------------------------
code_change(PreviousVersion, State, Extra) ->
  self() ! {code_change, PreviousVersion, State, Extra},
  {ok, State}.

%%--------------------- terminate  --------------------------------------
terminate(normal, _MapQueue) ->
  persian_event_server:stop(whereis(persian_event_server)),
  lager:info("- Requesting to stop persian_event_server"),
  lager:info("- Stoping persian_qu_server"),
  ok;
terminate(Reason, _State) ->
  lager:info("- Stoping persian_qu_server with reason ~p", [Reason]),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
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
  rpc:call(persian_node:event_node(Client), persian_event_server, notify_new_msg, [event_server_name(Client), Client]).
%%---- NO_MSG ------------------------------------
notify_no_msg(Client) ->
  rpc:call(persian_node:event_node(Client), persian_event_server, notify_no_msg, [event_server_name(Client), Client]).
%%---- PROCESS_MSG ------------------------------------
process_msg(Client, Msg) ->
  rpc:call(persian_node:event_node(Client), persian_event_server, process_msg, [event_server_name(Client), Client, Msg]).

event_server_name(Client) ->
  persian_node:event_server_name(persian_node:event_node(Client)).
