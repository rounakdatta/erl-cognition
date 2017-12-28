-module(record_passing_example_2). 
-export([start/0, process/1]). 

-record(kafka_message, {offset, magic_byte, attributes, key = <<"">>, value = <<"">>, crc}).

start() -> 
   Msg = #kafka_message{offset = 0, magic_byte = 0, attributes = 0, key = <<"hello">>, value = <<"world">>, crc = 12345678}, 
   io:format("~p~n", [Msg]),
   process(Msg).

process( #kafka_message{offset = OFFSET, magic_byte = MB, attributes = ATTR, key = KEY, value = VAL, crc = CRC}) ->
        io:format("~p ~p ~p ~p ~p ~p~n", [OFFSET, MB, ATTR, KEY, VAL, CRC]).    