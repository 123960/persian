{pend_msgs: [{ "client": "1"
                 "msgs": [] },
               {"client": "2",
                 "msgs": [] }
             ]}

{"1":[],"2":[],"3":[]}

[{<<"1">>, {[{<<"1">>, <<"<soapenv">>}, {<<"2">>, <<"<soapenv2">>}, {<<"3">>, <<"<soapenv3">>}], []}},{<<"2">>, {[{<<"1">>, <<"<soapenv">>}, {<<"2">>, <<"<soapenv2">>}, {<<"3">>, <<"<soapenv3">>}], []}}]

F=fun(MQ) -> orddict:fold(fun(K, V, Acc) -> orddict:store(K, queue:to_list(V), Acc) end,
                          orddict:new(),
                          MQ) end.
F([{<<"1">>, {[{<<"1">>, <<"<soapenv">>}, {<<"2">>, <<"<soapenv2">>}, {<<"3">>, <<"<soapenv3">>}], []}},{<<"2">>, {[{<<"1">>, <<"<soapenv">>}, {<<"2">>, <<"<soapenv2">>}, {<<"3">>, <<"<soapenv3">>}], []}}]).
F2=fun(ML, K1, K2) -> orddict:fold(fun(K, V, Acc) ->
                             orddict:store(K, {{K1, K}, {K2, V}}, Acc) end,
                           orddict:new(),
                           ML) end.
F2([{<<"1">>,
  [{<<"3">>,<<"<soapenv3">>},
   {<<"2">>,<<"<soapenv2">>},
   {<<"1">>,<<"<soapenv">>}]},
 {<<"2">>,
  [{<<"3">>,<<"<soapenv3">>},
   {<<"2">>,<<"<soapenv2">>},
   {<<"1">>,<<"<soapenv">>}]}], client, msgs).

F3=fun(MT) -> lists:map(fun({K, {C, M}}) -> [C, M] end, orddict:to_list(MT)) end.
F3([{<<"1">>,
  {{client,<<"1">>},
   {msgs,[{<<"3">>,<<"<soapenv3">>},
          {<<"2">>,<<"<soapenv2">>},
          {<<"1">>,<<"<soapenv">>}]}}},
 {<<"2">>,
  {{client,<<"2">>},
   {msgs,[{<<"3">>,<<"<soapenv3">>},
          {<<"2">>,<<"<soapenv2">>},
          {<<"1">>,<<"<soapenv">>}]}}}]).

F4=fun(M) ->
     lists:map(fun(M2) ->
                 lists:map(fun({K, V}) ->
                             case K of
                               msgs -> {msgs, F3(F2(V, msgId, content))};
                               _    -> {K, V}
                             end
                           end,
                           M2)
               end,
               M)
   end.

F4([[{client,<<"1">>},
  {msgs,[{<<"3">>,<<"<soapenv3">>},
         {<<"2">>,<<"<soapenv2">>},
         {<<"1">>,<<"<soapenv">>}]}],
 [{client,<<"2">>},
  {msgs,[{<<"3">>,<<"<soapenv3">>},
         {<<"2">>,<<"<soapenv2">>},
         {<<"1">>,<<"<soapenv">>}]}]]
).

[[{client,<<"1">>}, {msgs,[[{msgId,<<"1">>},{content,<<"<soapenv">>}]]}]]

jsone:encode(F4([[{client,<<"1">>},
  {msgs,[{<<"3">>,<<"<soapenv3">>},
         {<<"2">>,<<"<soapenv2">>},
         {<<"1">>,<<"<soapenv">>}]}],
 [{client,<<"2">>},
  {msgs,[{<<"3">>,<<"<soapenv3">>},
         {<<"2">>,<<"<soapenv2">>},
         {<<"1">>,<<"<soapenv">>}]}]]
)).

[
 {"client":"1",
 "msgs":[
         {"msgId":"1","content":"<soapenv"}
        ]}
]


[{<<"1">>,
  [{<<"1">>,
    [{sent,ok,1476558766507},{resp,nok,1476558766507}]},
   {<<"2">>,[{req,ok,1476558830524},{resp,nok,1476558830524}]},
   {<<"3">>,[{req,ok,1476558840526},{resp,nok,1476558860530}]},
   {<<"4">>,[{req,ok,1476558850529},{resp,nok,1476558860530}]},
   {<<"5">>,
    [{req,ok,1476558860530},{resp,nok,1476558860530}]}]},
 {<<"2">>,
  [{<<"2">>,
    [{req,ok,1476558802805},{resp,nok,1476558802806}]}]},
 {<<"3">>,
  [{<<"3">>,
    [{req,ok,1476558812808},{resp,nok,1476558812808}]}]}]
