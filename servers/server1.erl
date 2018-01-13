-module(server1).
-export([start/2, rpc/2]).

start(Name, Mod) ->
	register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)). 

	% the module which is spawned according to the received data is spawned and the status of the spawned process is registered along with its name

rpc(Name, Request) ->
	Name ! {self(), Request},

	% Name is the one I'm going to request so, I send it a message with my Pid (self()) and the Request obviously

	receive
		{Name, Response} -> Response

		% Response is printed whenever this pattern response is received

	end.

loop(Name, Mod, State) ->
	receive
		{From, Request} ->
			{Response, State1} = Mod:handle(Request, State),

			% loop just keeps looping and is the gateway for processing of requests. For the respective module, the handle function is called for processing and the result is stored as shown

			From ! {Name, Response},
			loop(Name, Mod, State1)

			% the new state that was received from the called module is initiated as the new loop and the old loop is terminated, so the loop gets terminated and redefined (restated actually) for each received message

		end.