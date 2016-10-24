%%%-------------------------------------------------------------------
%% @doc persian public API
%% @end
%%%-------------------------------------------------------------------

-module(persian_qu).

-behaviour(application).
-compile([{parse_transform, lager_transform}]).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  lager:start(),
  lager:info("- Starting persian_qu application"),
  persian_qu_supersup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  lager:info("- Stoping persian_qu application"),
  lager:stop(),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
