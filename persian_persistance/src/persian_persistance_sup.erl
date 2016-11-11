%%%-------------------------------------------------------------------
%% @doc persian persistance supervisor.
%% @end
%%%-------------------------------------------------------------------
-module(persian_persistance_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

%%====================================================================
%% API functions
%%====================================================================
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
init([]) -> {ok, {{one_for_one, 5, 5}, [{persian_persistance_node0,
                                          {persian_persistance_server, start_link, [persian_persistance_node0@localhost]},
                                           permanent,
                                           5000,
                                           worker,
                                           [persian_persistance_server]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
