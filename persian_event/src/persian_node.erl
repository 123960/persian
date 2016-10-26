%%%-------------------------------------------------------------------
%% @doc persian node_manager.
%% @end
%%%-------------------------------------------------------------------
-module(persian_node).
-export([event_httpc_acm_sup_name/0,event_server_sup_name/0,event_httpc_acm_server_name/0,
         event_server_name/0,qu_node/1,event_node/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------supervisors-------------------------------------------------
event_httpc_acm_sup_name() ->
  {ok, SNames} = application:get_env(persian_event, event_httpc_acm_sup_names),
  case orddict:find(node(), SNames) of
    {ok, SName}   -> SName;
    error         -> event_httpc_acm_sup0
  end.
event_server_sup_name() ->
  {ok, SNames} = application:get_env(persian_event, event_server_sup_names),
  case orddict:find(node(), SNames) of
    {ok, SName}   -> SName;
    error         -> event_server_sup0
  end.
%%------- servers -----------------------------------------------
event_httpc_acm_server_name() ->
  {ok, SNames} = application:get_env(persian_event, event_httpc_acm_server_names),
  case orddict:find(node(), SNames) of
    {ok, SName}   -> SName;
    error         -> event_httpc_acm_server0
  end.
event_server_name() ->
  {ok, SNames} = application:get_env(persian_event, event_server_names),
  case orddict:find(node(), SNames) of
    {ok, SName}   -> SName;
    error         -> event_server0
  end.
%%------- nodes -----------------------------------------------
qu_node(Client) ->
  {ok, QNodes} = application:get_env(persian_event, qu_nodes_per_pf),
  case orddict:find((Client rem 2), QNodes) of
    {ok, Node}    -> Node;
    error         -> persian_qu_node0@localhost
  end.
event_node(Client) ->
  {ok, ENodes} = application:get_env(persian_event, event_nodes_per_pf),
  case orddict:find((Client rem 2), ENodes) of
    {ok, Node}    -> Node;
    error         -> persian_event_node0@localhost
  end.

%%====================================================================
%% Internal functions
%%====================================================================
