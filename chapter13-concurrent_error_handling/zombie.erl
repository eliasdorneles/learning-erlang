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

zombie() ->
    io:format("I'm still alive~n"),
    timer:sleep(3000),
    zombie().

register_zombie() ->
    Pid = spawn(?MODULE, zombie, []),
    register(zombie, Pid),
    Pid.

monitor_zombie() ->
    on_exit(whereis(zombie),
            fun(_) ->
                    io:format("restarting zombie ...~n"),
                    register_zombie(),
                    monitor_zombie()
            end).

spawn_with_timeout(Mod, Func, Args, Time) ->
    Pid = my_spawn(Mod, Func, Args),
    spawn(fun() ->
                  timer:sleep(Time),
                  exit(Pid, killed)
          end),
    Pid.

my_spawn(Mod, Func, Args) ->
    Start = erlang:now(),
    Pid = spawn(Mod, Func, Args),
    on_exit(Pid,
            fun(Why) ->
                    End = erlang:now(),
                    io:format("Process died after ~p us, with cause: ~p~n", [timer:now_diff(End, Start), Why])
            end),
    Pid.
