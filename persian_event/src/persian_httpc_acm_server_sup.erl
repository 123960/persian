%%%-------------------------------------------------------------------
%% @doc persian event_server supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(persian_httpc_acm_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_one, 5, 5}, [{persian_httpc_acm_server1,
                                  {persian_httpc_acm_server, start_link, []},
                                   permanent,
                                   5000,
                                   worker,
                                   [persian_httpc_acm_server]}]} }.

%%====================================================================
%% Internal functions
%%====================================================================