-module(ringbenchmark).
-compile(export_all).

repass(Pid) ->
    receive Mesg ->
                %% io:format("repassing mesg ~p to ~p~n", [Mesg, Pid]),
                Pid ! Mesg,
                repass(Pid)
    end.
first_ring_loop(Fun) ->
    receive Mesg ->
                Fun(Mesg),
                first_ring_loop(Fun)
    end.

end_ring(Fun) ->
    spawn(?MODULE, first_ring_loop, [Fun]).

add_link(Pid) ->
    spawn(?MODULE, repass, [Pid]).

create_links(Pid, N) ->
    case N of
        1 -> Pid;
        N -> create_links(add_link(Pid), N-1)
    end.

create_ring(N) ->
    Fun = fun(Mesg) ->
                  io:format("received ~p~n", [Mesg]),
                  case Mesg of
                      {From, now, OldNow} ->
                          Diff = timer:now_diff(erlang:now(), OldNow),
                          From ! {self(), diff, Diff};
                      true -> void
                  end
          end,
    
    create_links(end_ring(Fun), N).

time_a_ring(N) ->
    Ring = create_ring(N),
    Ring ! {self(), now, erlang:now()},
    receive
        {_, diff, Diff} ->
            io:format("It took ~p microseconds~n", [Diff]),
            Diff;
        true -> void
    end.

