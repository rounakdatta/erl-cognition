-module(record_passing_example_1). 
-export([start/0, process/1]). 
-record(person, {name = "", id}). 

-record(pass, {arg1 ,
                name="",
                to_go=0}).

start() -> 
   P = #person{name = "John",id = 1}, 
   io:fwrite("~p~n",[P#person.id]), 
   io:fwrite("~p~n",[P#person.name]).  

process( #pass{arg1 = ARG1, name = NAME, to_go = TO_GO}) ->
        io:format("~p ~p ~p ~n", [ARG1,NAME,TO_GO]).    