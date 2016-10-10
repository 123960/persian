%%%-------------------------------------------------------------------
%% @doc persian qu_server supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(persian_http_api_server_sup).
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
  io:format("[persian_http_api_server_sup] - Iniciando persian_http_api_server na porta 3000.\n"),
  ElliOpts = [{callback, persian_http_api_server}, {port, 3000}],
  {ok, { {one_for_one, 5, 5}, [{http_api_server1,
                                {elli, start_link, [ElliOpts]},
                                 permanent,
                                 5000,
                                 worker,
                                 [elli]}]} }.

%%====================================================================
%% Internal functions
%%====================================================================
