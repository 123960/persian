%%%-------------------------------------------------------------------
%% @doc persian node_manager.
%% @end
%%%-------------------------------------------------------------------
-module(persian_event_nodem).
-export([event_httpc_acm_sup_name/0,event_server_sup_name/0,event_httpc_acm_server_name/0,
         event_server_name/0, event_server_name/1, qu_httpd_api_sup_name/0, qu_server_sup_name/0, qu_httpd_api_name/0,
         api_node_port/0, qu_server_name/0, qu_server_name/1, qu_node/1,event_node/1]).

%%====================================================================
%% API functions
%%====================================================================
%%-------- event supervisors-------------------------------------------------
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
%%------- event servers -----------------------------------------------
event_httpc_acm_server_name() ->
  {ok, SNames} = application:get_env(persian_event, event_httpc_acm_server_names),
  case orddict:find(node(), SNames) of
    {ok, SName}   -> SName;
    error         -> event_httpc_acm_server0
  end.
event_server_name() ->
  event_server_name(node()).
event_server_name(NodeName) ->
  {ok, SNames} = application:get_env(persian_event, event_server_names),
  case orddict:find(NodeName, SNames) of
    {ok, SName}   -> SName;
    error         -> event_server0
  end.
%%--------queue supervisors-------------------------------------------------
qu_httpd_api_sup_name() ->
  {ok, SNames} = application:get_env(persian_event, qu_httpd_api_sup_names),
  case orddict:find(node(), SNames) of
    {ok, SName}   -> SName;
    error         -> qu_httpd_api_sup0
  end.
qu_server_sup_name() ->
  {ok, SNames} = application:get_env(persian_event, qu_server_sup_names),
  case orddict:find(node(), SNames) of
    {ok, SName}   -> SName;
    error         -> qu_server_sup0
  end.
%%------- queue API ------------------------------------------------
qu_httpd_api_name() ->
  {ok, SNames} = application:get_env(persian_event, qu_httpd_api_names),
  case orddict:find(node(), SNames) of
    {ok, SName}   -> SName;
    error         -> httpd_api_server1
  end.
api_node_port() ->
  {ok, NPorts} = application:get_env(persian_event, qu_api_port_by_node),
  case orddict:find(node(), NPorts) of
    {ok, Port}    -> Port;
    error         -> 3000
  end.
%%------- queue servers -----------------------------------------------
qu_server_name() ->
  qu_server_name(node()).
qu_server_name(NodeName) ->
  {ok, SNames} = application:get_env(persian_event, qu_server_names),
  case orddict:find(NodeName, SNames) of
    {ok, SName}   -> SName;
    error         -> qu_server0
  end.
%%------- nodes -----------------------------------------------
qu_node(Client) ->
  {ok, QNodes} = application:get_env(persian_event, qu_nodes_per_pf),
  case orddict:find(pf(Client), QNodes) of
    {ok, Node}    -> Node;
    error         -> persian_qu_node0@localhost
  end.
event_node(Client) ->
  {ok, ENodes} = application:get_env(persian_event, event_nodes_per_pf),
  case orddict:find(pf(Client), ENodes) of
    {ok, Node}    -> Node;
    error         -> persian_event_node0@localhost
  end.

%%====================================================================
%% Internal functions
%%====================================================================
pf(Client) ->
  binary_to_integer(Client) rem 2.
