-module(persian_httpc_acm_server).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([init/1, handle_cast/2, handle_call/3, handle_info/2, code_change/3,
         terminate/2, start_link/0, stop/1, process_event/4]).

-define(ACM_URL, "http://172.22.4.142:8080/genericadapter/GenericAdapter").

init([]) ->
  lager:info("- Starting inets"),
  application:start(inets),
  lager:info("- Starting persian_httpc_acm_server"),
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
  lager:info("- [client:[~p]|msgid:[~p]] - Executing HTTP Request", [Client, MsgId]),
  case httpc:request(post, {?ACM_URL, [], [], Msg}, [], []) of
    {ok, Result}    -> lager:info("- [client:[~p]|msgid:[~p]] - Response OK in HTTP Request, starting response treatment", [Client, MsgId]),
                       send_resp(Client, MsgId, Result);
    {error, Reason} -> lager:warning("- [client:[~p]|msgid:[~p]|reason:] - Response NOK in HTTP Request, but I have nothing to do", [Client, MsgId, Reason])
  end,
  {reply, State, State};

%%--------------------- STOP handle_call --------------------------------
handle_call({terminate}, _From, State) ->
  {stop, normal, ok, State}.

%%--------------------- handle_info -------------------------------------
handle_info(Msg, State) ->
  lager:info("- [msgId:[~p]] - Receive message",[Msg]),
  {noreply, State}.

%%--------------------- CODE_CHANGE handle ------------------------------
code_change(PreviousVersion, State, Extra) ->
  self() ! {code_change, PreviousVersion, State, Extra},
  {ok, State}.

%%--------------------- terminate  --------------------------------------
terminate(normal, _State) ->
  io:format("- Stoping inets"),
  application:stop(inets),
  io:format("- Stoping persian_httpc_acm_server"),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
send_resp(Client, MsgId, Resp) ->
  persian_event_server:store_resp(whereis(persian_event_server), Client, MsgId, Resp).
