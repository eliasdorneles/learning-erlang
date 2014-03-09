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

iolist_to_atom(IoList) ->
    list_to_atom(lists:flatten(IoList)).

create_group_of_processes(N) ->
    spawn(fun() ->
                  for(1, N,
                      fun(I) ->
                              Mesg = io_lib:format("I'm a group process ~p~n", [I]),
                              Name = iolist_to_atom(io_lib:format("goo~w", [I])),
                              register(Name, spawn_link(fun() -> zombie(Mesg) end))
                      end),
                  receive after infinity -> true end
          end).

monitor_process_group(N) ->
    %% Create N linked processes that die together, and are all restarted when one of them dies
    Pid = create_group_of_processes(N),
    on_exit(Pid,
            fun(Why) ->
                    io:format("A process of the group died: ~p~n", [Why]),
                    io:format("Restarting all ~p processes...~n", [N]),
                    monitor_process_group(N)
            end).

keep_alive(Name, Fun) ->
    %% Creates a registered process that restarts when killed
    register(Name, Pid = spawn(Fun)),
    on_exit(Pid, fun(_Why) ->
                         io:format("~p died, restarting it...~n", [Name]),
                         keep_alive(Name, Fun) end),
    Pid.

monitor_some_processes(N) ->
    %% Create N processes that restart when killed
    for(1, N, fun(I) ->
                      Mesg = io_lib:format("I'm process ~p~n", [I]),
                      Name = iolist_to_atom(io_lib:format("zombie~w", [I])),
                      keep_alive(Name, fun() -> zombie(Mesg) end)
              end).

for(N, N, Fun) -> [Fun(N)];
for(I, N, Fun) -> [Fun(I)|for(I+1, N, Fun)].

zombie(Mesg) ->
    io:format(Mesg),
    timer:sleep(3000),
    zombie(Mesg).

zombie() ->
    zombie("I'm still alive~n").

register_zombie() ->
    keep_alive(zombie, fun() -> zombie("I'm still alive~n") end).

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
