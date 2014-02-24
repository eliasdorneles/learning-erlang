-module(procreation).
-compile(export_all).
%% -export([max/1]).


time_procs(N) ->
    statistics(runtime),
    statistics(wall_clock),
    L = for(1, N, fun() -> spawn(fun() -> wait() end) end),
    {_, Time1} = statistics(runtime),
    {_, Time2} = statistics(wall_clock),
    lists:foreach(fun(Pid) -> Pid ! die end, L),
    U1 = Time1 * 1000 / N,
    U2 = Time2 * 1000 / N,
    {U1, U2}.

wait() -> receive die -> void end.
for(N, N, F) -> [F()];
for(I, N, F) -> [F() | for(I+1, N, F)].

atom_to_integer(Atom) ->
      erlang:list_to_integer(erlang:atom_to_list(Atom)).

max([N]) when is_atom(N) ->
    max(atom_to_integer(N));
max(N) ->
    %% Creates N processes then destroy them
    %% See how much time this takes
    %% (adapted from Programming Erlang book)
    Max = erlang:system_info(process_limit),
    io:format("Maximum allowed processes: ~p~n", [Max]),
    {U1, U2} = time_procs(N),
    io:format("Process spawn time=~p (~p) microseconds -- with ~p processes~n", [U1, U2, N]).

%% generate output for plotting later
plot(M, N, Step) ->
    Times = lists:map(fun(X) -> {X, time_procs(X)} end, lists:seq(M, N, Step)),
    lists:foreach(fun({Max, {U1, U2}}) -> io:format("~p ~p ~p~n", [Max, U1, U2]) end, Times).

plot([M, N, Step]) ->
    %% handle arguments from cmdline
    plot(atom_to_integer(M), atom_to_integer(N), atom_to_integer(Step)).

