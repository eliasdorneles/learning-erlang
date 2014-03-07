-module(zombie).
-compile(export_all).

on_exit(Pid, Fun) ->
    spawn(fun() ->
                  Ref = monitor(process, Pid),
                  receive
                      {'DOWN', Ref, process, Pid, Why} ->
                          Fun(Why)
                  end
          end).

spawn_with_timeout(Mod, Func, Args, Time) ->
    Pid = spawn(Mod, Func, Args),
    timer:sleep(Time),
    exit(Pid, killed).

my_spawn1(Mod, Func, Args) ->
    spawn(fun() ->
                  Start = now(),
                  {Pid, Ref} = spawn_monitor(Mod, Func, Args),
                  receive
                      {'DOWN', Ref, process, Pid, Why} ->
                          End = now(),
                          io:format("Process ~p died after ~p us, with cause: ~p~n", [Ref, timer:now_diff(End, Start), Why])
                  end
          end).

my_spawn2(Mod, Func, Args) ->
    Start = erlang:now(),
    on_exit(spawn(Mod, Func, Args),
            fun(Why) ->
                    End = erlang:now(),
                    io:format("Process died after ~p us, with cause: ~p~n", [timer:now_diff(End, Start), Why])
            end).
