-module(lwes_test).

-export([start/1, 
        receive_events/2,
        match_values/3]).

start(Args) -> 
  lwes:start(),
  {JsonFile, N, Port, LangPorts} = parse_args(Args),
  Event=event_from_json(JsonFile),
  ets:new(results_tab, [set, named_table, public]),
  Channel = channel(listener, "127.0.0.1", Port),
  lwes:listen (Channel, fun receive_events/2, tagged, [event_from_json(JsonFile)]),
  send_loop(Event, LangPorts, N),
  lwes:close(Channel),
  Results = ets:match(results_tab, '$1'),
  ets:delete(results_tab), 
  halt(parse_results(Results, LangPorts)).  

parse_args(Args) -> 
  {"/Users/vikram.kadi/work/open-source-repos/lwes-test/jsons/testevent.json", 
   60, 2336, [{java, 2335}]}.

parse_results(Results, LangPorts) -> 
  case length(Results) of
    0 -> io:format("Did not recieve any messages from any languages~n", []), 1;
    X when X =/= length(LangPorts) -> io:format("Some of the language tests never sent a response~n",[]);
    _ -> Failures = lists:filter(
                        fun([{Language, Result}]) 
                          -> false == Result end, Results),
         case length(Failures) of
          0 -> io:format("Messages from all languages validated~n"), 0;
          _ -> io:format("Some of the messages could not be read"), 1
         end
  end.

receive_events(E, A) -> 
  io:format("Received the event ~p~n", [E]),
  [Source] = A,
  case validate_event(E, Source) of
    error -> A;
    {Language, Result} -> ets:insert(results_tab, {Language, Result})
  end,
  A.

validate_event(E, A) ->
  {lwes_event, _, Attrs} = E, 
  {lwes_event, _, Attrs1} = A, 
  case get_field(Attrs, <<"language">>) of 
    {string, Language} -> {Language, match_fields(Attrs, Attrs1)};
    {notfound, _} -> error
  end.

match_fields([], Attrs) -> true;
match_fields([{Type, Key, Value} | Rest], Attrs) 
  when Key =:= <<"language">>; Key =:= <<"SenderIP">>;
       Key =:= <<"SenderPort">>; Key =:= <<"ReceiptTime">>
    -> match_fields(Rest, Attrs);
match_fields([{Type, Key, Value} | Rest], Attrs)
  -> {Type2, Value2} = get_field(Attrs, Key),
      case {Type2, Value2} of
        {notfound, _} -> io:format("Did not find the element ~p~n",[Key]), false;
        {T, _} when T =/= Type -> io:format("Found the element ~p but types don't match ~n",[Key]), false;
        {T, V} when T =:= Type2 
          ->  case match_values(T, Value, V) of
                true -> io:format("Element ~p matched, moving to the next element~n",[Key]), match_fields(Rest, Attrs);
                false -> io:format("Element ~p values ~p and ~p do not match ~n",[Key, Value, V]),false
              end
      end.

match_values(T, [], []) -> true;
match_values(T, [V1 | R1], [V2 | R2]) 
  when T == float_array; T == double_array; 
       T == nullable_float_array; T == nullable_double_array
       -> case match_values(double, V1, V2) of
            true -> match_values(T, R1, R2);
            false -> false
          end;
match_values(T, [V1 | R1], [V2 | R2]) 
  when T == string_array; T == nullable_string_array
       -> case match_values(string, V1, V2) of
            true -> match_values(T, R1, R2);
            false -> false
          end;
match_values(T, undefined, undefined) when T == double; T == float; T == string
    -> true;
match_values(T, V1, V2) when T == double; T == float -> 
  case abs(V2 - V1) of
    Epsilon when Epsilon > 0.0001 -> false;
    _ -> true
  end;
match_values(T, V1, V2) when T == string -> V1 =:= list_to_binary(V2); 
match_values(T, V1, V2) -> V1 =:= V2.

get_field([], Key) -> {notfound, notfound};
get_field([{Type, Key1, Value}| Rest], Key) when Key =:= Key1
  -> {Type, Value};
get_field([_H | T], Key) -> get_field(T, Key). 
  
send_loop(Event, LangPorts, N) -> 
  lists:map(fun(_X) -> emit_on_ports(LangPorts, Event),
              timer:sleep(1000) end
           ,lists:seq(1, N)).
      

emit_on_ports(LangPorts, Event) -> 
  lists:map(fun({Lang, Port}) -> 
              Channel = channel(emitter, "127.0.0.1", Port), 
              lwes:emit(Channel, lwes_event:set_string(Event, "language", Lang)),
              lwes:close(Channel) end, 
            LangPorts).

channel(Type, Ip, Port) -> {ok, Channel} = lwes:open(Type, {Ip, Port}), Channel.

event_from_json(File) -> 
  {ok, Contents} = file:read_file(File), 
  lwes_event:from_json(Contents).


