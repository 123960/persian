%%%-------------------------------------------------------------------
%% @doc persian top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(persian_qu_supersup).

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
    supervisor:start_link({global, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 0, 1}, [{persian_node:qu_httpd_api_sup_name(),
                                   {persian_httpd_api_server_sup, start_link, []},
                                    permanent,
                                    5000,
                                    supervisor,
                                    [persian_httpd_api_server_sup]},
                                  {persian_node:qu_server_sup_name(),
                                   {persian_qu_server_sup, start_link, []},
                                    permanent,
                                    5000,
                                    supervisor,
                                    [persian_qu_server_sup]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
