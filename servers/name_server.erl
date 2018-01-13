-module(name_server).
-export([init/0, add/2, whereis/1, handle/2]).
-import(server1, [rpc/2]).

% server1 was imported as we'll be using functions from that file; the only function that we'll be using is rpc/2

add(Name, Place) -> rpc(name_server, {add, Name, Place}).
whereis(Name) -> rpc(name_server, {whereis, Name}).

% these are the only functions that the client can use; these are sent in packeted format to the server1 which again makes furthur calls from there to here as shown below

init() -> dict:new().

% a new dictionary is created when the init callback is made from server1.erl on start/2 being called. Dicts will not survive restarts; but they hold data within memory for the runtime. Stackoverflow recommends using mnesia, dets for permanent (type of) storage

handle({add, Name, Place}, Dict) ->  {ok, dict:store(Name, Place, Dict)};
handle({whereis, Name}, Dict) -> {dict:find(Name, Dict), Dict}.