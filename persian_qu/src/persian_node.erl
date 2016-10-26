%%%-------------------------------------------------------------------
%% @doc persian node_manager.
%% @end
%%%-------------------------------------------------------------------
-module(persian_node).
-export([qu_httpd_api_sup_name/0, qu_server_sup_name/0, qu_httpd_api_name/0,
         api_node_port/0, qu_server_name/0, qu_node/1, event_node/1]).
%%====================================================================
%% API functions
%%====================================================================
%%--------supervisors-------------------------------------------------
qu_httpd_api_sup_name() ->
  {ok, SNames} = application:get_env(persian_qu, qu_httpd_api_sup_names),
  case orddict:find(node(), SNames) of
    {ok, SName}   -> SName;
    error         -> qu_httpd_api_sup0
  end.
qu_server_sup_name() ->
  {ok, SNames} = application:get_env(persian_qu, qu_server_sup_names),
  case orddict:find(node(), SNames) of
    {ok, SName}   -> SName;
    error         -> qu_server_sup0
  end.
%%------- API ------------------------------------------------
qu_httpd_api_name() ->
  {ok, SNames} = application:get_env(persian_qu, qu_httpd_api_names),
  case orddict:find(node(), SNames) of
    {ok, SName}   -> SName;
    error         -> httpd_api_server1
  end.
api_node_port() ->
  {ok, NPorts} = application:get_env(persian_qu, qu_api_port_by_node),
  case orddict:find(node(), NPorts) of
    {ok, Port}    -> Port;
    error         -> 3000
  end.
%%------- servers -----------------------------------------------
qu_server_name() ->
  {ok, SNames} = application:get_env(persian_qu, qu_server_names),
  case orddict:find(node(), SNames) of
    {ok, SName}   -> SName;
    error         -> qu_server0
  end.
%%------- nodes -----------------------------------------------
qu_node(Client) ->
  {ok, QNodes} = application:get_env(persian_qu, qu_nodes_per_pf),
  case orddict:find((Client rem 2), QNodes) of
    {ok, Node}    -> Node;
    error         -> persian_qu_node0@localhost
  end.
event_node(Client) ->
  {ok, ENodes} = application:get_env(persian_qu, event_nodes_per_pf),
  case orddict:find((Client rem 2), ENodes) of
    {ok, Node}    -> Node;
    error         -> persian_event_node0@localhost
  end.

%%====================================================================
%% Internal functions
%%====================================================================
