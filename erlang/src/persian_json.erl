%%%-------------------------------------------------------------------
%% @doc persian json treatment
%%%-------------------------------------------------------------------
-module(persian_json).
-export([mq_to_json/1, event_to_json/1]).

%%====================================================================
%% API functions
%%====================================================================
mq_to_json([]) -> jsone:encode({[{<<"pend_msgs">>, []}]});
mq_to_json(MQ) -> jsone:encode({[{<<"pend_msgs">>,
                                 append_subkeys(qu, append_keys(mq_to_mlist(MQ), client, msgs))
                                }]}).
event_to_json([]) -> jsone:encode({[{<<"pend_msgs">>, []}]});
event_to_json(ME) -> jsone:encode({[{<<"processed_msgs">>,
                                    append_subkeys(event, append_keys(ME, client, msgs))
                                  }]}).

%%====================================================================
%% Internal functions
%%====================================================================
%%Yeah I know, this is horrible, some day I will adjust
%%---Transforms a map of a queue in a map of a list
mq_to_mlist(MQ) ->
  orddict:fold(fun(K, V, Acc) -> orddict:store(K, queue:to_list(V), Acc) end,
               orddict:new(),
               MQ).
%%---Appends keys to first level
append_keys(ML, K1, K2) ->
  orddict:fold(fun(K, V, Acc) -> [[{K1, K}, {K2, V}]] ++ Acc end,
               [],
               ML).
append_keys(ML, msgId, operation, statusReq, statusResp, content, operTimestamp) ->
  orddict:fold(fun(MsgId, [{Oper, StatusReq, StatusResp, Content, OperTimestamp}], Acc) ->
                 [[{msgId, MsgId}, {operation, Oper}, {statusReq, StatusReq}, {statusResp, StatusResp}, {content, Content}, {operTimestamp, OperTimestamp}]] ++ Acc end,
               [],
               ML).
%%---Appends keys to second level
append_subkeys(Element, M)  ->
  lists:map(fun(M2) ->
              lists:map(fun({K, V}) ->
                          case K of
                            msgs -> case Element of
                                      qu    -> {msgs, append_keys(V, msgId, content)};
                                      event -> {msgs, append_keys(V, msgId, operation, statusReq, statusResp, content, operTimestamp)}
                                    end;
                            _    -> {K, V}
                          end
                        end,
                        M2)
            end,
            M).
