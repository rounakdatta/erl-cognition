-module(area_server).
-export([start/0, loop/0, sending/2]).

start() ->
	spawn(fun loop/0).

sending(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} ->
			Response
	end.

loop() ->
	receive
		{From, {rectangle, Width, Height}} ->
			From ! {self(), Width * Height},
			loop();
		{From, {circle, R}} ->
			From ! {self(), 3.14 * R * R},
			loop();
		{From, Other} ->
			From ! {self(), error, Other},
			loop()
	end.