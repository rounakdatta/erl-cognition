-module(rosogolla).
-behaviour(gen_server).

-export([start_link/0, hungry_call/4]).
-export([init/1, handle_call/3, handle_cast/2]).

-record(rosogolla, {type, size, sweetness=100}).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

hungry_call(Pid, Type, Size, Sweetness) ->
	gen_server:call(Pid, {quickly_bring, Type, Size, Sweetness}).

init([]) -> {ok, []}.

handle_call({quickly_bring, Type, Size, Sweetness}, _From, Handi) ->
	if Handi =:= [] ->
		{reply, pickupnpack(Type, Size, Sweetness), Handi};
		Handi =/= [] ->
		{reply, hd(Handi), tl(Handi)}
	end;

handle_call(terminate, _From, Handi) ->
	{stop, normal, ok, Handi}.

handle_cast({return, Rosogolla = #rosogolla{}}, Handi) ->
	{noreply, [Rosogolla|Handi]}.

pickupnpack(Type, Size, Sweetness) ->
	#rosogolla{type=Type, size=Size, sweetness=Sweetness}.