%%%-------------------------------------------------------------------
%% @doc persian json treatment
%%%-------------------------------------------------------------------
-module(persian_json).
-export([mq_to_json/1]).

%%====================================================================
%% API functions
%%====================================================================
mq_to_json([]) -> jsone:encode({[{<<"pend_msgs">>, []}]});
mq_to_json(MQ) -> jsone:encode({[{<<"pend_msgs">>,
                                 f4(f3(f2(f(MQ), client, msgs)))
                                }]}).

%%====================================================================
%% Internal functions
%%====================================================================
f(MQ) -> orddict:fold(fun(K, V, Acc) -> orddict:store(K, queue:to_list(V), Acc) end,
                      orddict:new(),
                      MQ).
f2(ML, K1, K2) -> orddict:fold(fun(K, V, Acc) ->
                    orddict:store(K, {{K1, K}, {K2, V}}, Acc) end,
                  orddict:new(),
                  ML).
f3(MT) -> lists:map(fun({_K, {C, M}}) -> [C, M] end, orddict:to_list(MT)).
f4(M)  ->
  lists:map(fun(M2) ->
              lists:map(fun({K, V}) ->
                        case K of
                           msgs -> {msgs, f3(f2(V, msgId, content))};
                           _    -> {K, V}
                         end
                       end,
                       M2)
            end,
            M).
