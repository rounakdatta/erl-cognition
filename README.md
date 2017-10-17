   Quick example of how message sending-and-receiving is done in erlang
   
   ```erl-sh
   > self().
     <0.720.0>  -- Process id
   > self() ! {hello, world}, ok.
      ok
   > self() ! {hello, world}, ok.
      ok
   > flush().        // Prints what you had done in the session
     Shell got {hello, world}   
     Shell got {hello, world}
     ok
