%%%-------------------------------------------------------------------
%% @doc persian qu_server supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(persian_httpd_api_server_sup).
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
  io:format("- Starting persian_httpd_api_server on port 3000"),
  ElliOpts = [{callback, persian_httpd_api_server}, {port, 3000}],
  {ok, { {one_for_one, 5, 5}, [{httpd_api_server1,
                                {elli, start_link, [ElliOpts]},
                                 permanent,
                                 5000,
                                 worker,
                                 [elli]}]} }.

%%====================================================================
%% Internal functions
%%====================================================================
