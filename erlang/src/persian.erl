%%%-------------------------------------------------------------------
%% @doc persian public API
%% @end
%%%-------------------------------------------------------------------

-module(persian).

-behaviour(application).
-compile([{parse_transform, lager_transform}]).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  lager:start(),
  lager:info("- Starting persian"),
  persian_supersup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  lager:info("- Stoping persian"),
  lager:stop(),
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
