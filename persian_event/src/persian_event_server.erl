%%%-------------------------------------------------------------------
%% @doc persian event_server.
%% State holds processed messages in a map by client and by msgid
%% State = [{Client1, [{MsgId1, [{Oper, StatusReq, StatusResp, Content, OperTimestamp}]}, {MsgId2, [{Oper, StatusReq, StatusResp, Content, OperTimestamp}]}...]},
%%          {Client2, [{MsgId1, [{Oper, StatusReq, StatusResp, Content, OperTimestamp}]}, {MsgId2, [{Oper, StatusReq, StatusResp, Content, OperTimestamp}]}...]},
%%              ...]
%% Oper       = [req, resp]
%% StatusReq  = [ok, nok]
%% StatusResp = [0, AnyErrorInDestiny]
%% @end
%%%-------------------------------------------------------------------
-module(persian_event_server).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3,
         terminate/2, start_link/0, stop/0, get_all_msgs/0, get_client_msgs/1, get_msg/2,
         notify_new_msg/1, notify_no_msg/1, process_msg/2, store_resp/4]).

init([]) ->
  lager:info("- Starting persian_event_server"),
  {ok, []}.

%%====================================================================
%% API functions
%%====================================================================
start_link()                            -> gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).
stop()                                  -> gen_server:call({global, ?MODULE}, {terminate}).
get_all_msgs()                          -> gen_server:call({global, ?MODULE}, {get_all_msgs}).
get_client_msgs(Client)                 -> gen_server:call({global, ?MODULE}, {get_client_msgs, Client}).
get_msg(Client, MsgId)                  -> gen_server:call({global, ?MODULE}, {get_msg, Client, MsgId}).
notify_new_msg(Client)                  -> gen_server:cast({global, ?MODULE}, {new_msg, Client}).
notify_no_msg(Client)                   -> gen_server:cast({global, ?MODULE}, {no_msg, Client}).
process_msg(Client, Msg)                -> gen_server:cast({global, ?MODULE}, {process_msg, Client, Msg}).
store_resp(Result, Client, MsgId, Resp) -> gen_server:cast({global, ?MODULE}, {store_resp, Result, Client, MsgId, Resp}).

%%====================================================================
%% Callback Handlers
%%====================================================================
%%--------------------- NEW_MSG handle_cast (async) --------------------
handle_cast({new_msg, Client}, State) ->
  self() ! {new_msg, Client},
  request_new_msg(Client),
  {noreply, State};

%%--------------------- NO_MSG handle_cast (async) --------------------
handle_cast({no_msg, Client}, State) ->
  self() ! {no_msg, Client},
  {noreply, State};

%%--------------------- PROCESS_MSG handle_cast (async) --------------------
handle_cast({process_msg, Client, {MsgId, Msg}}, State) ->
  self() ! {process_msg, Client},
  lager:info("- [MsgId:[~p]] - Processing message", [MsgId]),
  lager:info("- [MsgId:[~p]] - Looking for the message in cache", [MsgId]),
  case State of
    []    -> lager:info("- [MsgId:[~p]] - Cache is empty, sending message", [MsgId]),
             process_event(Client, MsgId, Msg),
             request_new_msg(Client),
             {noreply, orddict:store(Client, orddict:store(MsgId, [{req, ok, null, <<"Content">>, get_timestamp()}], orddict:new()), State)};
    _else -> case orddict:find(Client, State) of
               error      -> lager:info("- [MsgId:[~p]] - There is no message in the cache to the client, sending message", [MsgId]),
                             process_event(Client, MsgId, Msg),
                             request_new_msg(Client),
                             {noreply, orddict:store(Client, orddict:store(MsgId, [{req, ok, null, <<"Content">>, get_timestamp()}], orddict:new()), State)};
               {ok, Msgs} -> case orddict:find(MsgId, Msgs) of
                               error   -> lager:info("- [MsgId:[~p]] - Message not processed, sending message", [MsgId]),
                                          process_event(Client, MsgId, Msg),
                                          request_new_msg(Client),
                                          {noreply, orddict:store(Client, orddict:store(MsgId, [{req, ok, null, <<"Content">>, get_timestamp()}], Msgs), State)};
                               {ok, _} -> lager:info("- [MsgId:[~p]] - Message already processed, nothing to do", [MsgId]),
                                          request_new_msg(Client),
                                          {noreply, orddict:store(Client, orddict:store(MsgId, [{dup, null, null, <<"Content">>, get_timestamp()}], Msgs), State)}
                             end
             end
  end;

handle_cast({store_resp, Result, Client, MsgId, _Resp}, State) ->
  self() ! {store_resp, Client},
  lager:info("- [MsgId:[~p]|Client:[~p]] - Processing response for a message", [MsgId, Client]),
  case State of
    []    -> lager:warning("- [MsgId:[~p]|Client:[~p]] - Cache is empty, something is wrong", [MsgId, Client]),
             {noreply, State};
    _else -> case orddict:find(Client, State) of
               error      -> lager:warning("- [MsgId:[~p]|Client:[~p]] - There is no message of the client, something is wrong", [MsgId, Client]),
                             {noreply, State};
               {ok, Msgs} -> case orddict:find(MsgId, Msgs) of
                               error         ->
                                 lager:warning("- [MsgId:[~p]|Client:[~p]] - Unprocessed message, something is wrong", [MsgId, Client]),
                                 {noreply, State};
                               {ok, MsgInfo} ->
                                 lager:info("- [MsgId:[~p]|Client:[~p]] - Persisting response", [MsgId, Client]),
                                 {noreply, orddict:store(Client, orddict:store(MsgId, lists:append(MsgInfo, [{resp, Result, ok, <<"Content">>, get_timestamp()}]), Msgs), State)}
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
               {ok, Msgs} -> {reply, orddict:store(Client, Msgs, orddict:new()), State}
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
  lager:info("- [msg:~p] - Receive message", [Msg]),
  {noreply, State}.

%%--------------------- CODE_CHANGE handle ------------------------------
code_change(PreviousVersion, State, Extra) ->
  self() ! {code_change, PreviousVersion, State, Extra},
  {ok, State}.

%%--------------------- terminate  --------------------------------------
terminate(normal, _State) ->
  lager:info("- Stoping persian_event_server"),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
request_new_msg(Client) ->
  rpc:call(persian_node:qu_node(Client), persian_qu_server, async_dequeue, [Client]).
process_event(Client, MsgId, Msg) ->
  persian_httpc_acm_server:process_event(whereis(persian_httpc_acm_server), Client, MsgId, Msg, infinity).
get_timestamp() ->
  {Mega, Sec, Micro} = os:timestamp(),
  (Mega*1000000 + Sec)*1000 + round(Micro/1000).
