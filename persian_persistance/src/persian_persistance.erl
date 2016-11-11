%%%-------------------------------------------------------------------
%% @doc persian persistance.
%% Persistance module
%%%-------------------------------------------------------------------
-module(persian_persistance).
-behaviour(application).
-export([create_db/1, start/2, stop/1]).
-compile([{parse_transform, lager_transform}]).
-record(persian_mapqueue, {client, msgs=[]}).

%%====================================================================
%% API functions
%%====================================================================
start(normal, []) ->
  lager:start(),
  lager:info("- Starting mnesia"),
  application:set_env(mnesia, dir, "./persian_database"),
  application:start(mnesia),
  lager:info("- Waiting mnesia tables"),
  mnesia:wait_for_tables([persian_mapqueue], 5000),
  lager:info("- Starting persian_persistance"),
  persian_persistance_sup:start_link().

create_db(_Nodes) ->
  application:set_env(mnesia, dir, "./persian_database"),
  mnesia:create_schema([node()]),
  application:start(mnesia),
  mnesia:create_table(persian_mapqueue,
                      [{attributes, record_info(fields, persian_mapqueue)},
                       {disc_copies, [node()]}]),
  application:stop(mnesia).

stop(_) ->
  application:stop(mnesia),
  lager:stop().
%%====================================================================
%% Internal functions
%%====================================================================
