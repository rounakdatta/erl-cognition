   Quick example of how message sending-and-receiving is done in erlang
   
   ```erl-sh
   > self().
     <0.720.0>    %Process ID of the current bash shell
   > self() ! {hello, world}, ok.
      ok
   > self() ! {just, fun}, ok.
      ok
   > flush().     %Prints what you had done in the session
     Shell got {hello, world}   
     Shell got {just, fun}
     ok
   > self() ! {new, example}, ok.
     ok
   > self() ! {another, one}, ok.
     ok
   > self() ! {trying, out, exps}, done.
     done
   > F = fun() -> receive X -> X end end.    %The function which helps in receiving messages one-by-one
     #Fun<erl_eval.20.99386804>
   > F().      %Received 1st message
     {new,example}
   > F().      %Received 2nd message
     {another,one}
   > F().      %Received 3rd message
     {trying,out,exps}
