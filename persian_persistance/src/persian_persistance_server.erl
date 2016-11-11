%%%-------------------------------------------------------------------
%% @doc persian persistance server.
%% Persistance module
%%%-------------------------------------------------------------------
-module(persian_persistance_server).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-export([init/1, terminate/2, start_link/1, code_change/3, handle_call/3, stop/1,
         sync_persistance_queue/3, sync_get_queue/2, sync_get_all/1]).
-record(persian_mapqueue, {client, msgs=[]}).

%%====================================================================
%% API functions
%%====================================================================
init([]) ->
  lager:info("- Starting persian_persistance_server"),
  {ok, []}.

start_link(PName)                            -> gen_server:start_link({global, PName}, ?MODULE, [], []).
stop(PName)                                  -> gen_server:call({global, PName}, {terminate}).
sync_persistance_queue(PName, Client, Queue) -> gen_server:call({global, PName}, {p_queue, Client, Queue}).
sync_get_queue(PName, Client)                -> gen_server:call({global, PName}, {get_queue, Client}).
sync_get_all(PName)                          -> gen_server:call({global, PName}, {get_all}).

%%====================================================================
%% Callback Handlers
%%====================================================================
%%--------------------- Enqueue handle_call ----------------------------
handle_call({p_queue, Client, Queue}, _From, []) ->
  persist_queue(Client, Queue),
  {reply, ok, []};

handle_call({get_queue, Client}, _From, []) ->
  {reply, get_queue(Client), []};

handle_call({get_all}, _From, []) ->
  {reply, get_all(), []}.

code_change(_PreviousVersion, State, _Extra) ->
  {ok, State}.

%%--------------------- terminate  --------------------------------------
terminate(normal, _MapQueue) ->
  lager:info("- Stoping persian_persistance_server"),
  ok;
terminate(Reason, _State) ->
  lager:info("- Stoping persian_persistance_server with reason ~p", [Reason]),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
persist_queue(Client, Msgs) ->
  F = fun() ->
        mnesia:write(#persian_mapqueue{client=Client,
                                       msgs=Msgs})
      end,
  mnesia:activity(transaction, F).

get_queue(Client) ->
  F = fun() ->
        case mnesia:read({persian_mapqueue, Client}) of
          [#persian_mapqueue{client=C, msgs=Msgs}] -> [{C, Msgs}];
          _not_found   -> []
        end
      end,
  mnesia:activity(transaction, F).

get_all() ->
  CatchAll = [{'_',[],['$_']}],
  F = fun() -> mnesia:select(persian_mapqueue, CatchAll) end,
  mnesia:transaction(F).
