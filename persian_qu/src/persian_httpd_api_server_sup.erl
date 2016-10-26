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
    supervisor:start_link({global, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  io:format("- Starting persian_httpd_api_server on port ~p", [persian_node:api_node_port()]),
  ElliOpts = [{callback, persian_httpd_api_server}, {port, persian_node:api_node_port()}],
  {ok, { {one_for_one, 5, 5}, [{persian_node:qu_httpd_api_name(),
                                {elli, start_link, [ElliOpts]},
                                 permanent,
                                 5000,
                                 worker,
                                 [elli]}]} }.

%%====================================================================
%% Internal functions
%%====================================================================
